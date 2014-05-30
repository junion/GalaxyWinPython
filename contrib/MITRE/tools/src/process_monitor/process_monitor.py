# This file (c) Copyright 1998 - 2002 The MITRE Corporation
# 
# This file is part of the Galaxy Communicator system. It is licensed
# under the conditions described in the file LICENSE in the root 
# directory of the Galaxy Communicator system.

"""Process monitor

The process monitor controls an arbitrary number of processes. It 
permits a nuber of arguments for each process, of which -c
terminates each argument sequence. 

--compressed: start in compressed configuration
--ncols <num>: how many columns to display the panes in
     (also the number of rows of buttons in compressed mode)
-T <title>: title of process
--open: start the monitor with this process's window open
--start: start the monitor with this process started
--keep_alive: restart this process if it dies
--input_line: provide a line input pane
--input_return: provide a button to send a newline
-c <cmdline>: command line for this process

The first element of the command line must be something that can 
be preceded by 'exec'. Example:

process_monitor -T "Add and multiply" --open --start \
    -c "run_server bin/addmultiply" -T "Hub" --open \
    -c "bin/hub -pgm_file addmultiply.pgm"
"""

import os, sys, socket, string, signal, select, math, time

import Tkinter
from Tkinter import *

########################################
#
# GUI units
#
########################################

BadLabelJustification = 'BadLabelJustification'

HEADERFONT = '-*-lucida-bold-r-normal-sans-14-140-*'
CONTENTSFONT = '-*-lucida-medium-r-normal-sans-12-120-*'

class LabeledEntry(Frame):
    def __init__(self, master, label_text,
		 entry_value = '',
		 entry_width = 20,
		 return_cmd = None,
		 label_width = None):
	Frame.__init__(self, master)
	if label_width:
	    fontvar = StringVar()
	    fontvar.set(label_text)
	    self.label = Entry(self, relief = 'flat',
			       borderwidth = 0,
			       font = HEADERFONT, justify = 'left',
			       textvariable = fontvar,
			       width = label_width, state = 'disabled')
	else:
	    self.label = Label(self, text=label_text, font= HEADERFONT)
	self.entry = Entry(self, borderwidth=2, relief='sunken',
			   font=CONTENTSFONT, width=entry_width)
	if type(entry_value) is StringType:
	    self.textvar = StringVar()
	else:
	    self.textvar = IntVar()
	self.textvar.set(entry_value)
	self.entry['textvariable']=self.textvar
	if return_cmd:
	    self.entry.bind('<KeyPress-Return>',
			    lambda event, cmd = return_cmd, s = self: \
			    cmd(s.GetValue()))
	else:
	    self.entry['state'] = 'disabled'
	self.label.pack(side = 'left')
	self.entry.pack(side = 'left', fill = 'x', expand = 1)
    def Enable(self):
	self.entry['state'] = 'normal'
    def GetValue(self):
	return self.textvar.get()
    def SetValue(self, val):
	self.textvar.set(val)

# Sam 11/5/99: We need to enforce line breaks, lest the
# process monitor choke on extra-long lines. The Hub does exactly
# this, by printing out its wait dots without line breaks.
# After each print, I'll probe the current position, and if it's greater
# than the width of the widget, I'll print a line break. 

class ScrollPane(Frame):
    def __init__(self, master, height = 12, width = 60,
		 label = '', label_justification = 'left',
		 action_button_label = None, action_button_action = None):
	Frame.__init__(self, master)
	if label:
	    self.labelframe = Frame(self)
	    self.labelframe.pack(side = 'top', expand = 0, fill = 'x')	    
	    self.label = Label(self.labelframe, text=label, font= HEADERFONT)
	    if label_justification == 'left':
		# self.label.pack(side = 'top', anchor = 'w')
		self.label.pack(side = 'left')
		if action_button_label or action_button_action:		
		    self.button = Button(self.labelframe,
					 text = action_button_label,
					 font = HEADERFONT,
					 command = action_button_action)
		    self.button.pack(side = 'right')
	    elif label_justification == 'right':
		# self.label.pack(side = 'top', anchor = 'e')
		self.label.pack(side = 'right')
		if action_button_label or action_button_action:		
		    self.button = Button(self.labelframe,
					 text = action_button_label,
					 font = HEADERFONT,
					 command = action_button_action)
		    self.button.pack(side = 'left')
	    elif label_justification == 'center':
		self.label.pack(side = 'top')
	    else:
		raise BadLabelJustification, label_justification
	    self.textframe = Frame(self)
	    self.textframe.pack(side = 'top', expand = 1, fill = 'both')
	    if (action_button_label or action_button_action) and \
	       label_justification == 'center':
		self.button = Button(self,
				     text = action_button_label,
				     font = HEADERFONT,
				     command = action_button_action)
		self.button.pack(side = 'top')
	else:
	    self.textframe = self
	self.textbox = Text(self.textframe, borderwidth=1, relief='sunken',
			    state='disabled', height=height, width=width,
			    font= CONTENTSFONT
			    )
	self.scrollbar = Scrollbar(self.textframe, borderwidth=1,
				   relief='sunken',
				   command=self.textbox.yview)
	self.scrollbar.pack(side = "right", fill = "y")
	self.textbox['yscrollcommand'] = self.scrollbar.set
	self.textbox.pack(side='left', expand=1, fill='both')	
    def write(self, text):
	self.Write(text)
    def Write(self, text):
        # For some bizarre reason, when I send Windows output (\r\n)
        # to the widget, I get \r showing up as a string character
        # (i.e., Tkinter doesn't seem to be dealing with this
        # very well). So we need to replace it.
        # Actually, it appears that this is a general problem,
        # when we read from something that isn't a file descriptor;
        # it appears that \r\n is converted to \n when we read
        # in a file in Python using open(). So I think I need
        # to move this to the Windows specific stuff.
	self.textbox['state']='normal'
	self.textbox.insert('end', text)
	self.textbox.yview_pickplace('end')
	# Get the current end.
	line, pos = string.splitfields(self.textbox.index('end - 1 chars'), ".")
	# We'll stick with the requested width, because
	# figuring out the actual width is too damn hard.
	if string.atoi(pos) >= string.atoi(self.textbox['width']):
	    self.textbox.insert('end', '\n')
	    self.textbox.yview_pickplace('end')                
	self.textbox['state']='disabled'
        return string.atoi(line)
    def Clear(self):
	self.textbox['state']='normal'
	self.textbox.delete('0.0', 'end')
	self.textbox['state']='disabled'

UnknownMenuType = 'UnknownMenuType'

class MyMenu(Menu):
    def __init__(self, master, vals_pairs):
	Menu.__init__(self, master, font= CONTENTSFONT, tearoff=0)
	for key, val in vals_pairs:
	    type, trueval = val
	    if type == 'command':
		self.add('command', label = key,
			 command = lambda com = trueval: com())
	    elif type == 'cascade':
		self.add('cascade', label = key,
			 menu = MyMenu(self, trueval))
	    else:
		raise UnknownMenuType

class MenuBarEntry(Menubutton):    
    def __init__(self, master, text, vals_pairs):
	Menubutton.__init__(self, master, font = CONTENTSFONT,
			    text = text)
	self.menu = MyMenu(self, vals_pairs)
	self['menu'] = self.menu

class Error(Frame):
    def __init__(self, master, error_string):
	self.master = master
	
	Frame.__init__(self, master)
	self['borderwidth'] = 2
	self['relief'] = 'ridge'
	self.pack(side='top')
	self.master.title('You should know...')
	
	self.msg = Message(self, text=error_string, width=200,
			   font= CONTENTSFONT,
			   justify='center')
	self.msg.pack(side = 'top')
	self.dismiss_row = Frame(self)	
	self.dismiss_row.pack(side='top')
	self.dismiss_row['borderwidth'] = 2
	self.CLOSE = Button(self.dismiss_row, text='OK',
			    font= HEADERFONT,
			    command=self.close_cmd)
	self.CLOSE.pack(side='left')
	self.pack(fill = 'both')
	
    def close_cmd(self):
	self.master.destroy()

###################################
#
# Toplevel monitor window
#
###################################

import popen2

NoCmdlineError = "NoCmdlineError"

HISTORYFONT = '-*-courier-medium-r-normal-sans-12-120-*'

class StatusButton(Label):
    def __init__(self, *args, **kw):
	apply(Label.__init__, (self,) + args, kw)
	self.visible = 1
	self.configure_text = "(stopped)"
	Label.configure(self, text = self.configure_text)
    def configure(self, text = None, command = None):
	if text == "Start":
	    self.configure_text = "(stopped)"
	elif text == "Stop":
	    self.configure_text = "(running)"
	if self.visible:
	    self.MakeVisible()
    def MakeVisible(self):
	Label.configure(self, text = self.configure_text)
	self.visible = 1
    def MakeInvisible(self):
	Label.configure(self, text = (9 * " "))
	self.visible = 0

# This is the window which displays the process. It contains
# a frame which has all the controls in it. In addition,
# it has a header for the command and a show/hide button.

class ReferenceEntry(Frame):
    def __init__(self, master, entry_host, process):
	Frame.__init__(self, master)
	self.entry_host = entry_host
	header_host = Frame(self)
	header_host.pack(side = 'top', anchor = 'w')
	self.header = Frame(header_host)
	self.empty_header = Frame(header_host)
	self.show_button = Button(self.header, text = "Show",
				  relief = "groove",
				  command = lambda s = self: s.Enlarge())
	self.show_button.pack(side = 'left')
	cmd = process.title
	# This is now 71, because the other 9 will come from
	# the run button.
	label = Label(self.header, text = cmd  + ((71 - len(cmd)) * " "),
		      font = HISTORYFONT,
		      justify = 'left', 
		      relief = "flat")
	label.pack(side = 'left')
	self.header.pack(side = 'top', anchor = 'w')
	self.pframe = SingleProcessFrame(self, process = process)
	self.run_button = StatusButton(self.header, relief = "flat")
	self.pframe.run_buttons.append(self.run_button)
	self.run_button.pack(side = "right")

    def Enlarge(self):
	# Reset the geometry.
	self.entry_host.master.geometry("")
	self.pframe.pack(side = 'top', anchor = 'w',
			 fill = 'both', expand = 1)
	# Remove the text for the run buttons.
	self.run_button.MakeInvisible()
	self.show_button.configure(text = "Hide",
				   command = self.Reduce)

    def Reduce(self):
	# Reset the geometry.
	self.entry_host.master.geometry("")
	self.pframe.forget()
	# Show the text for the run button.
	self.run_button.MakeVisible()
	self.show_button.configure(text = "Show",
				   command = self.Enlarge)

    def UseCompressedConfig(self):
	# Convert to the compressed configuration. Enlarge and
	# get rid of the header line.
	self.header.forget()
	self.empty_header.pack(side = 'top', anchor = 'w')
	# Make sure that it's enlarged.
	self.Enlarge()

    def UseColumnConfig(self):
	self.empty_header.forget()
	self.header.pack(side = 'top', anchor = 'w')

class ProcessFrame(Frame):
    def __init__(self, process_env, process_set, master = None):
	Frame.__init__(self, master)
	self['borderwidth'] = 2
        # Default title.
        self.master.title("Process monitor")
	self.pack(fill = 'both', expand = 1)
	self.top = Frame(self)
	self.top.pack(side = 'top', fill = 'x', anchor = 'w')
	self.topmenubar = Frame(self.top, relief = 'groove', borderwidth = 2)
	self.topmenubar.pack(side = 'top', fill = 'x', anchor = 'w')
	b1 = MenuBarEntry(self.topmenubar, "File",
			  [("Quit", ('command', self.quit))])
	b1.pack(side = 'left')
	b2 = MenuBarEntry(self.topmenubar, "Process Control",
			  [("Stop all", ('command', self.Shutdown)),
			   ("Clear all", ('command', self.Clear)),
			   ("Restart all", ('command', self.Restart))])
	b2.pack(side = "left")
	b3 = MenuBarEntry(self.topmenubar, "Configuration",
			  [("Column configuration",
			    ('cascade',
			     [("One column",
			       ('command',
				lambda s = self: s.UseColumnConfig(1))),
			     ("Two columns",
			      ('command',
			       lambda s = self: s.UseColumnConfig(2))),
			     ("Three columns",
			      ('command',
			       lambda s = self: s.UseColumnConfig(3)))])),
			   ("Compressed configuration",
			    ('cascade',
			     [("One button row",
			       ('command',
				lambda s = self: s.UseCompressedConfig(1))),
			      ("Two button rows",
			       ('command',
				lambda s = self: s.UseCompressedConfig(2))),
			      ("Three button rows",
			       ('command',
				lambda s = self: s.UseCompressedConfig(3)))]))])
	b3.pack(side = "left")
        all_sets = []
        for p in process_env.process_sets:
            if p.meets_requirements:
                f = lambda s = self, p_set = p: s.UseProcessSet(p_set)
                all_sets.append((p.title, ('command', f)))
        if len(all_sets) > 1:
            b4 = MenuBarEntry(self.topmenubar, "Process Set", all_sets)
            b4.pack(side = "left")
	bottom = Frame(self)
	bottom.pack(side = 'left', anchor = 'n',
		    fill = 'both', expand = 1)
	self.pane_frame = Frame(bottom)
	self.pane_frame.pack(side = 'top', anchor = 'w',
			     fill = 'both', expand = 1)
 	# Don't pack the clone button, either, until the
 	# compressed config is used.
 	self.clone_button = Button(bottom,
 				   text = "Detach this pane",
 				   relief = 'groove',
 				   font = HEADERFONT,
 				   command = self.Detach)
	# Now, we build the buttons, but we don't pack them.
	self.button_row = Frame(self.top, relief = 'groove', borderwidth = 2)
	self.processes = []
	self.num_nondetached_processes = 0
	self.active_process = None
	self.separator_list = []
	self.process_env = process_env
	# For temporary files.
	self.process_env.Initialize()
        self.process_set = None
        self.UseProcessSet(process_set)

    def UseProcessSet(self, new_set):

        # First, shut down and delete all windows, if
        # appropriate.
        if self.process_set:
            # Shutdown the process set.
            self.Shutdown()
            # Unuse all the currently used elements.
            if self.compressed:
                self.UnuseCompressedConfig()
            else:
                self.UnuseColumnConfig()
            # Destroy all the currently used elements.
            for s in self.separator_list:
                s.destroy()
            self.separator_list = []
            for p in self.process_set.processes:
                if p.reference_entry:
                    p.reference_entry.destroy()
                    p.reference_entry = None
                p.compressed_button.destroy()
                p.compressed_button = None

        # Now, set up the new configuration.

        if new_set:
            self.process_set = new_set
            self.master.title(self.process_set.title)
            # Now, we build the individual panes and buttons.
            for p in self.process_set.processes:
                # Build a frame which has the lower pane as a hidden
                # component.
                # Set up the reference line.
                p.AddTk(self.tk)
                self.AttachProcess(p)
                if p.open:
                    p.reference_entry.Enlarge()
                if p.start:
                    p.window_host.Restart()
                if p.keepalive:
                    p.window_host.KeepAlive()
                # Now, build the button.
                b = Button(self.button_row,
                           text = p.title,
                           relief = 'raised',
                           font = HEADERFONT)
                p.compressed_button = b
                b.configure(command = lambda p = p, s = self:
                            s.MakeButtonActive(p))
            for i in range(len(self.process_set.processes) - 1):
                # Build as many separators as we need.
                self.separator_list.append(Frame(self.pane_frame,
                                                 relief = 'sunken',
                                                 height = 2,
                                                 borderwidth = 1))
            self.detached_entries = []
            self.numcols = 0
            self.compressed = 0
            if self.process_env.compressed:
                self.UseCompressedConfig(self.process_env.ncols)
            else:
                self.UseColumnConfig(self.process_env.ncols)

    def AttachProcess(self, p):
	ref = ReferenceEntry(self.pane_frame, self, p)
	p.SetReferenceEntry(ref)
	self.num_nondetached_processes = self.num_nondetached_processes + 1
    
    def UnuseColumnConfig(self):
	for s in self.separator_list:
	    s.grid_forget()
	for p in self.process_set.processes:
	    if p.window_host and p.Nondetached():
		p.reference_entry.grid_forget()
	
    def UseCompressedConfig(self, ncols, force = 0):
	if self.compressed and (ncols == self.numcols) and (not force):
	    return
	if (not self.compressed):
	    if not force: self.UnuseColumnConfig()
	    self.compressed = 1
	# We want to use the gridder instead of the
	# packer for the buttons.
	cols, rows = self.button_row.grid_size()
	# First, get rid of all the weights.
	for i in range(cols):
	    self.button_row.grid_columnconfigure(i, weight = '0')
	for i in range(rows):
	    self.button_row.grid_rowconfigure(i, weight = '0')
	# Now, compute the buttons per button row.
	buttons_per_button_row = int(math.ceil(float(self.num_nondetached_processes)/float(ncols)))
	i = j = 0
	for p in self.process_set.processes:
	    if p.Nondetached():
		p.reference_entry.UseCompressedConfig()
		# Pack every compressed button.		
		p.compressed_button.grid(row = j, column = i,
					 sticky = 'news')
		i = i + 1
		if (i % buttons_per_button_row) == 0:
		    j = j + 1
		    i = 0
	cols, rows = self.button_row.grid_size()
	# Now, restore the weights.
	for i in range(cols):
	    self.button_row.grid_columnconfigure(i, weight = '1')
	for i in range(rows):
	    self.button_row.grid_rowconfigure(i, weight = '1')
	self.numcols = rows
	
	cols, rows = self.pane_frame.grid_size()
	for i in range(cols):
	    self.pane_frame.grid_columnconfigure(i, weight = '0')
	for i in range(rows):
	    self.pane_frame.grid_rowconfigure(i, weight = '0')
	self.MakeButtonActive(self.process_set.processes[0])
	self.button_row.pack(side = 'top', anchor = 'w', fill = 'x')
	# We only pack the clone button if there is more than
	# one pane.
	if self.num_nondetached_processes > 1:
	    self.clone_button.pack(side = 'bottom', anchor = 'w', fill = 'x')
	    
    # These two methods are specific to the compressed configuration.
    def MakeButtonEntryInactive(self, p):
	if p.Nondetached():
	    if p.reference_entry:
		p.reference_entry.grid_forget()
	    p.compressed_button.configure(relief = 'raised')
    
    def MakeButtonActive(self, process):
	for entry in self.process_set.processes:
	    self.MakeButtonEntryInactive(entry)
	if process:
	    self.active_process = process
	    process.compressed_button.configure(relief = 'sunken')
	    process.reference_entry.grid(row = '0',
					 column = '0', sticky = 'news')
	    self.pane_frame.grid_columnconfigure(0, weight = '1')
	    self.pane_frame.grid_rowconfigure(0, weight = '1')
    
    def Reattach(self, p, old_toplevel):
	# Reattaching is tricky, because you have to regenerate
	# using the current configuration. First, you build
	# a new reference entry which has the right pane as
	# its parent, Then, you have to
	# unforget the button and regenerate the grid. Because
	# the processes remain in order, they'll get regenerated
	# in the same order.
	self.AttachProcess(p)
	p.SetDetached(0)
	old_toplevel.destroy()
	p.reference_entry.Enlarge()
	# Now, we need to regenerate things.
	if self.compressed:
	    # Regenerate the compressed list.
	    self.UnuseCompressedConfig()
	    self.UseCompressedConfig(self.numcols, force = 1)
	else:
	    # Regenerate the uncompressed structure.
	    self.UnuseColumnConfig()
	    self.UseColumnConfig(self.numcols, force = 1)
	    
    def Detach(self):
	# First, make another button active.
	if self.active_process:
	    # Get rid of the button.
	    p = self.active_process
	    p.compressed_button.grid_forget()
	    self.update()
	    # Deactivate the button entry.
	    self.MakeButtonEntryInactive(p)
	    # Make it detached.
	    p.SetDetached(1)
	    self.num_nondetached_processes = self.num_nondetached_processes - 1
	    # Activate the first nondetached element.
	    for next_p in self.process_set.processes:
		if next_p.Nondetached():
		    self.MakeButtonActive(next_p)
		    break
	    # Now,the complicated part. We want to detach the
	    # process from the old ref_entry and give it
	    # to a new reference entry in a new toplevel.
	    # There will be no reattach option at this point.
	    # The window reattachment doesn't work quite
	    # right yet.
	    new_top = Toplevel()
	    new_top.protocol('WM_DELETE_WINDOW', lambda s = self, p = p, t = new_top: s.Reattach(p, t))
	    f = Frame(new_top)
	    f.pack(fill = 'both', expand = '1')
	    new_ref_entry = ReferenceEntry(f, new_top, p)
	    p.SetReferenceEntry(new_ref_entry)
	    new_ref_entry.pack(side = 'top', fill = 'both', expand = '1')
	    reattach_button = Button(f,
				     text = "Reattach this pane",
				     relief = 'groove',
				     font = HEADERFONT,
				     command = lambda s = self, p = p, t = new_top: s.Reattach(p, t))
	    reattach_button.pack(side = 'top', fill = 'x')
	    new_top.title(p.title)
	    new_ref_entry.UseCompressedConfig()
	    if self.num_nondetached_processes == 1:
		# If we only have one pane left, don't
		# allow it to be detached.
		self.clone_button.forget()
    
    def UnuseCompressedConfig(self):
	self.button_row.forget()
	self.clone_button.forget()
	self.active_process = None
	for p in self.process_set.processes:
	    p.compressed_button.grid_forget()
	    if p.reference_entry and p.Nondetached():
		p.reference_entry.grid_forget()	
	for s in self.separator_list:
	    s.grid_forget()
    
    def UseColumnConfig(self, ncols, force = 0):
	if self.compressed == 0 and ncols == self.numcols and (not force):
	    return
	# Make it uncompressed	
	if self.compressed:
	    if not force: self.UnuseCompressedConfig()
	    self.compressed = 0
	# First, get rid of all the weights.
	cols, rows = self.pane_frame.grid_size()
	for i in range(cols):
	    self.pane_frame.grid_columnconfigure(i, weight = '0')
	for i in range(rows):
	    self.pane_frame.grid_rowconfigure(i, weight = '0')
	panes_per_column = int(math.ceil(float(self.num_nondetached_processes)/float(ncols)))
	i = j = 0
	for p in self.process_set.processes:
	    if not p.Nondetached():
		continue
	    p.reference_entry.UseColumnConfig()
	    # Add a separator above every nonzeroth element.
	    if i > 0:
		self.separator_list[i - 1].grid(row = ((i * 2) - 1), column = j, sticky = 'ew')
	    p.reference_entry.grid(row = (i * 2), column = `j`,
				   sticky = 'news')
	    i = i + 1
	    if (i % panes_per_column) == 0:
		j = j + 1
		i = 0	
	cols, rows = self.pane_frame.grid_size()
	for i in range(cols):
	    self.pane_frame.grid_columnconfigure(i, weight = '1')
	for i in range(rows):
	    self.pane_frame.grid_rowconfigure(i, weight = '1')
	self.numcols = ncols

    def Shutdown(self):
	# Don't really kill. Collect the pids and then do the really kill
	# afterward.
        if self.process_set:
            all_handles = []        
            for p in self.process_set.processes:
                handle = p.Shutdown(really_kill = 0)
                if handle is not None: all_handles.append(handle)
            if all_handles:
                self.process_set.EnforceShutdown(all_handles)

    def Restart(self):
	for p in self.process_set.processes:
	    p.Restart()

    def Clear(self):
	for p in self.process_set.processes:
	    p.Clear()
    	
    def quit(self):
	# self.timer.deletetimerhandler()
	self.Shutdown()
	# For temporary files.
	self.process_env.Cleanup()
	self.master.destroy()
	self.master.quit()

    def force_quit(self):
	self.quit()

    def Poll(self):
	for p in self.process_set.processes:
	    p.Poll()
	self.timer = self.tk.createtimerhandler(500, self.Poll)

    def Run(self):
	# self.timer = self.tk.createtimerhandler(500, self.Poll)
	self.mainloop()

# We adopt a more complex model, where all the info about
# the process lives in a ProcessContainer, which can be
# made to point to different windows (looking forward to the
# point where we reuse a single pane for multiple processes).

# This callback structure has to tolerate the possibility
# that we'll have either timer callbacks or file
# callbacks.

def _CreateHandleCallback(handle, process_container):
    if process_container.FDCallbackAvailable():
        return FDHandleCallback(handle, process_container)
    else:
        return TimerHandleCallback(handle, process_container)

class HandleCallback:
    def __init__(self, handle, process_container):
        self.handle = handle
        self.process = process_container

# I don't think the writes are every used.

class FDHandleCallback(HandleCallback):
    def __init__(self, handle, process_container):
        HandleCallback.__init__(self, handle, process_container)
        if handle == process_container.out_handle:
            self.fh_mask = Tkinter.WRITABLE
            self.cb = lambda s, m, h = handle, o = self: o.process.WriteCallback(h)
        else:
	    self.fh_mask = Tkinter.READABLE
            # Read as much as you can.
            self.cb = lambda s, m, h = handle, o = self: o.process.ReadCallback(h, -1)
        self.process.tk.createfilehandler(handle, self.fh_mask, self.cb)
        self.has_filehandler = 1
    def Remove(self):
        if self.has_filehandler:
            self.process.tk.deletefilehandler(self.handle.fileno())
            self.has_filehandler = 0

class TimerHandleCallback(HandleCallback):
    def __init__(self, handle, process_container):
        HandleCallback.__init__(self, handle, process_container)
        if handle == process_container.out_handle:
            self.cb = self.WriteTimer
        else:
	    self.cb = self.ReadTimer
        self.timer = self.process.tk.createtimerhandler(10, self.cb)
    def WriteTimer(self):
        self.process.WriteCallback(self.handle)
        self.timer = self.process.tk.createtimerhandler(10, self.WriteTimer)
    def ReadTimer(self):
        self.process.ReadCallback(self.handle, -1)
        if self.timer is not None:
            # It may have been removed as a result of shutting down.
            self.timer = self.process.tk.createtimerhandler(10, self.cb)
    def Remove(self):
        if self.timer:
            self.timer.deletetimerhandler()
            self.timer = None

# The process container is the object which houses the
# process and all the information associated with it. The
# window host for the process is where the output goes.
# When we set a new window host, we destroy the old one;
# window hosts are never reused. There will be no need
# for a dummy process, because windows without processes
# won't exist. It may be possible to have processes without
# windows, but we still need to track the process. Crucial
# information about the process includes the title, the
# command line, and the input_mode.

from basic_process_monitor import ProcessContainer, \
     ProcessContainerSet, ProcessEnvironment, ConfigurationError

class TkProcessContainer(ProcessContainer):
    config_table = ProcessContainer.config_table + \
		   [(None, "--input_line", 0, "LineInputMode", 0),
		    (None, "--input_return", 0, "ReturnInputMode", 0),
		    (None, "--start", 0, "SetAutoStart", 0),
		    (None, "--open", 0, "SetVisible", 0),
		    ("PROCESS_MONITOR_ARGS:", None, 1, \
		     "AddProcessMonitorArgs", 0)]

    def __init__(self, set):
	ProcessContainer.__init__(self, set)
	self.input_mode = None
	self.open = 0
	self.start = 0
	self.window_host = None
	self.reference_entry = None
	self.detached = 0

    # Configuration methods.
	
    def LineInputMode(self):
	if self.input_mode is not None:
	    raise ConfigurationError, "input mode already specified"
	self.input_mode = "input_line"

    def ReturnInputMode(self):
	if self.input_mode is not None:
	    raise ConfigurationError, "input mode already specified"
	self.input_mode = "input_return"

    def SetAutoStart(self):
	self.start = 1

    def SetVisible(self):
	self.open = 1

    def AddCommandLine(self, cmdline):
	ProcessContainer.AddCommandLine(self, cmdline)
	self.WriteCommandLine()

    def ConfigDone(self):
	ProcessContainer.ConfigDone(self)
	if not self.title:
	    if len(self.cmdline) > 60:
		self.title = self.cmdline[:57] + "..."
	    else:
		self.title = self.cmdline

    def AddProcessMonitorArgs(self, args):
	# Just splitting the args isn't good enough, because
	# the args are expecting to be in a command line. That
	# means that there may be quoted material in the lists.
	# Grrrr.
        cmdline_args = self.TokenizeCmdlineArgs(args)
	env = self.process_set.process_environment
	# Just use the local configuration.
	expanded_dict = {}
	env._AddConfigTable(self.__class__, expanded_dict)
	optlist, ignore = env._DigestConfiguration(cmdline_args,
						   expanded_dict,
						   1, 0)
	for key, value in optlist:
	    cur_set, cur_container = env._ConfigureOption(self.process_set,
							  self, key, value,
							  1, expanded_dict)
	    # If we ever reach the end, barf.
	    if cur_container is None:
		raise ConfigurationError, "process monitor args can't close block"
	
    # Runtime methods.
    
    def _SetupHandles(self):
        self.callback_dict = {}
        ProcessContainer._SetupHandles(self)

    def AddTk(self, tk):
        self.tk = tk

    def SetDetached(self, detached):
	self.detached = detached
    
    def Nondetached(self):
	return self.detached == 0

    def Clear(self):
	if self.window_host is not None:
	    self.window_host.Clear()
	
    def UpdateIdle(self):
	if self.window_host is not None:
	    # Don't update(), just update_idletasks().
	    # If you update(), constant reads will
	    # lock the display.
	    self.window_host.update_idletasks()

    def SetReferenceEntry(self, ref):
	old_ref = self.reference_entry
	self.SetWindowHost(ref.pframe)
	if old_ref is not None:
	    old_ref.destroy()
	self.reference_entry = ref
    
    def SetWindowHost(self, host):
	old_host = self.window_host
	if old_host is not None:
	    self.UnmonitorOutput()
	    # And destroy it.
	    old_host.destroy()
	self.window_host = host
	if (old_host is not self.window_host) and \
	   (host is not None):
	    # Update the new host.
	    self.window_host.SetProcess(self)	    
	    # Also, monitor the output, I think.
	    self.MonitorOutput()

    def Pause(self):
	self.UnmonitorOutput()
	if self.window_host:
	    self.window_host.LocalPause()

    def Resume(self):
	self.MonitorOutput()
	if self.window_host:
	    self.window_host.LocalResume()

    def KeepAlive(self):
	ProcessContainer.KeepAlive(self)
	if self.window_host:
	    self.window_host.LocalKeepAlive()

    def LetDie(self):
	ProcessContainer.LetDie(self)
	if self.window_host:
	    self.window_host.LocalLetDie()
    
    def UnmonitorOutput(self):
        for cb in self.callback_dict.values():
            cb.Remove()
        self.callback_dict = {}

    def MonitorOutput(self):
        if self.in_handle:
            cb = _CreateHandleCallback(self.in_handle, self)
            self.callback_dict[self.in_handle] = cb
        if self.err_handle:
            cb = _CreateHandleCallback(self.err_handle, self)
            self.callback_dict[self.err_handle] = cb

    def WriteHistory(self, str, fd):
	ProcessContainer.WriteHistory(self, str, fd)
	if self.window_host is not None:
	    cur_line = self.window_host.history.Write(str)
	    # If the clear history instruction is a number,
	    # treat it as the number of lines to save.
            if (self.process_set.process_environment.clear_history is not None) and \
               cur_line > self.process_set.process_environment.clear_history:
                self.ClearHistory()

    def WriteCommandLine(self):
	self.WriteHistory(("[%s]\n" % self.WrapCmdlineForExecution(self.GetCmdlineForDisplay(), self.interleave_error)), None)

    def ClearHistory(self):
	ProcessContainer.ClearHistory(self)
	if self.window_host is not None:
	    self.window_host.LocalClearHistory()
    
    def Shutdown(self, really_kill = 1):
	p = ProcessContainer.Shutdown(self, really_kill)
	if self.window_host:
	    self.window_host.LocalShutdown()
	return p
    
    # In order to interleave stdout and stderr better, I've
    # redirected stderr to stdout in the subshell. The problem is
    # that under this configuration, the shell does not return
    # a valid fd for the error stream. So I need not to monitor it.

    def Restart(self):
	ProcessContainer.Restart(self)
	if self.window_host:
	    self.window_host.LocalRestart()

    def GetCmdlineForDisplay(self):
	if self.window_host:
	    self.window_host.UpdateCmdline()
	return ProcessContainer.GetCmdlineForDisplay(self)

    def GetCmdlineForExecution(self):
	if self.window_host:
	    self.window_host.UpdateCmdline()
	return ProcessContainer.GetCmdlineForExecution(self)

    def SetCmdline(self, str):
	ProcessContainer.SetCmdline(self, str)
	if self.window_host:
	    self.window_host.LocalSetCmdline(ProcessContainer.GetCmdlineForDisplay(self))

    def _ForceShutdown(self, handle):
        if self.callback_dict.has_key(handle):
            self.callback_dict[handle].Remove()
            del self.callback_dict[handle]
        ProcessContainer._ForceShutdown(self, handle)

class TkProcessContainerSet(ProcessContainerSet):
    def __init__(self, environment):
	ProcessContainerSet.__init__(self, environment)
	self.AddTitle("Process monitor")

class TkProcessEnvironment(ProcessEnvironment):    
    config_table = ProcessEnvironment.config_table + \
		   [("NUM_DIVISIONS:", "--ncols", 1, "AddNumDivisions", 0),
		    ("CLEAR_HISTORY:", "--clear_history", 1,
		     "AddClearHistory", 0),
		    ("COMPRESSED:", "--compressed", 0, "AddCompressed", 0)]
    def __init__(self):
	ProcessEnvironment.__init__(self,
				    container_set_class = TkProcessContainerSet,
				    container_class = TkProcessContainer)
	self.ncols = 1
	self.compressed = 0	
	self.clear_history = None

    def AddProcessSet(self, p_set):
        ProcessEnvironment.AddProcessSet(self, p_set)

    def AddNumDivisions(self, data):
	try:
	    self.ncols = string.atoi(data)
	except:
	    raise ConfigurationError, "ill-formed --ncols argument"

    def AddCompressed(self):
	self.compressed = 1
    
    def AddClearHistory(self, instructions):
	try:
	    self.clear_history = string.atoi(instructions)
	except:
	    raise ConfigurationError, \
		  ("bad clear_history value %s" % instructions)

# This will have a button line at the bottom.

class SingleProcessFrame(Frame):
    
    def __init__(self, master = None, process = None):
	Frame.__init__(self, master)
	self.process = None
	self.history = ScrollPane(self,
				  width = 80,
				  height = 10)
	self.cmdline_typein = LabeledEntry(self, "Command", entry_value = "")
	self.cmdline_typein.entry.configure(font = HISTORYFONT)
	self.cmdline_typein.Enable()
	self.history.textbox.configure(font = HISTORYFONT)
	# Adding a cmdline typein.
	if process.input_mode == "input_line":
	    self.typed_input = LabeledEntry(self, "Input",
					    return_cmd = self.InputCallback)
	    self.typed_input.entry.configure(font = HISTORYFONT)
	elif process.input_mode == "input_return":
	    self.typed_input = Frame(self)
	    self.input_button = Button(self.typed_input,
				       text = "Input <return>",
				       relief = "groove",
				       command = self.InputReturn)
	    self.input_button.pack(side = 'left')
	else:
	    self.typed_input = Frame(self)
	# Pack the typed input first, so that it doesn't disappear
	# when you resize.
	self.typed_input.pack(side = 'bottom', anchor = 'w',
			      fill = 'x', expand = 0)
	self.cmdline_typein.pack(side = 'bottom', anchor = 'w',
				 fill = 'x', expand = 0)
	self.history.pack(side = 'bottom', anchor = 'w',		  
			  fill = 'both', expand = 1)
	self.pause_button = Button(self.typed_input, text = "Pause output",
				   relief = "groove",
				   command = self.Pause)
	self.pause_button.pack(side = 'right')
	self.clear_button = Button(self.typed_input, text = "Clear",
				   relief = "groove",
				   command = self.Clear)
	self.clear_button.pack(side = 'right')
	self.keepalive_button = Button(self.typed_input, text = "Keep alive",
				       relief = "groove",
				       command = self.KeepAlive)
	self.keepalive_button.pack(side = 'right')
	if process.alive:
	    self.run_buttons = [Button(self.typed_input, text = "Stop",
				       relief = "groove",
				       command = self.Shutdown)]
	else:
	    self.run_buttons = [Button(self.typed_input, text = "Start",
				       relief = "groove",
				       command = self.Restart)]
	self.run_buttons[0].pack(side = 'right')

    def KeepAlive(self):
	self.process.KeepAlive()

    def LocalKeepAlive(self):
	self.keepalive_button.configure(text = "Let die",
					command = self.LetDie)

    def LetDie(self):
	self.process.LetDie()

    def LocalLetDie(self):
	self.keepalive_button.configure(text = "Keep alive",
					command = self.KeepAlive)

    def SetProcess(self, p):
	self.process = p
	self.history.Clear()
	self.history.Write(p.history_buffer)
	self.LocalSetCmdline(p.cmdline)

    def UpdateCmdline(self):
	self.process.SetCmdline(self.cmdline_typein.GetValue())

    def Clear(self):
	self.process.ClearHistory()
	self.process.WriteCommandLine()

    def LocalClearHistory(self):
	self.history.Clear()

    def Pause(self):
	self.process.Pause()

    def LocalPause(self):
	self.pause_button.configure(text = "Resume output",
				    command = self.Resume)

    def Resume(self):
	self.process.Resume()

    def LocalResume(self):
	self.pause_button.configure(text = "Pause output",
				    command = self.Pause)
	
    def Shutdown(self, really_kill = 1):
	return self.process.Shutdown(really_kill)

    def LocalShutdown(self):
	self.pause_button.configure(text = "Pause output",
				    command = self.Pause)
	if self.run_buttons:
	    for run_button in self.run_buttons:
		run_button.configure(text = "Start",
				     command = self.Restart)

    def Restart(self):
	self.process.Restart()
	
    def LocalRestart(self):
	self.LocalShutdown()
	self.LocalResume()
	if self.run_buttons:
	    for run_button in self.run_buttons:
		run_button.configure(text = "Stop",
				     command = self.Shutdown)

    def InputCallback(self, str):
	self.typed_input.SetValue("")
	if not str:
	    str = '\n'
	elif str[-1] != '\n':
	    str = str + '\n'
	# Write it to the history!
	self.process.WriteHistory(str, None)
	self.__WriteString(str)

    def __WriteString(self, str):
	self.process.WriteString(str)

    def SetCmdline(self, str):
	self.process.SetCmdline()

    def LocalSetCmdline(self, str):
	self.cmdline_typein.SetValue(str)

    def InputReturn(self):
	# Write it to the history!
	self.process.WriteHistory('\n', None)
	self.__WriteString('\n')

#########################################
#
# main()
#
#########################################

import getopt

# I'm not going to try to generate the usage string. I'll
# just need to remember to update it.

def Usage():
    print "Usage: process_monitor [ --ncols <num> ] [ --compressed ] [ --clear_history <lines> ] [ --master_title <title> ] [ [ -T <title> ] [ --open ] [ --start ] [ --keep_alive ] [ --input_line | --input_return ] -c <cmdline> ]*\n       process_monitor config_file [config_num_or_name] [-- <arg>*]"
    sys.exit(1)

def main():
    try:
	env = TkProcessEnvironment()
	env.DigestCommandLine(sys.argv[1:])
    except ConfigurationError, text:
	print "Error:", text
	Usage()
    try:
        config = env.ChooseConfig()
    except ConfigurationError, text:
        print text
        Usage()
    f = ProcessFrame(env, config)
    signal.signal(signal.SIGINT, lambda n, stack, s = f: s.force_quit())
    signal.signal(signal.SIGTERM, lambda n, stack, s = f: s.force_quit())
    if hasattr(signal, "SIGQUIT"):
        # Doesn't exist on Windows.
        signal.signal(signal.SIGQUIT, lambda n, stack, s = f: s.force_quit())
    try:
	f.Run()
    except "foo":
	print "Encountered an unrecoverable error of type %s with argument %s. Exiting." % (`sys.exc_type`, `sys.exc_value`)
	sys.stdout.flush()
	f.quit()

main()
