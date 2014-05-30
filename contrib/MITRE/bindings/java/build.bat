@echo off
REM --------------------------------------------------------------------------
REM This MS-DOS batch file compiles the Galaxy Communicator Java bindings and 
REM creates a JAR file in the local lib directory.
REM --------------------------------------------------------------------------

REM --------------------------------------------------------------------------
REM Set the path to your Java 2 installation.
REM --------------------------------------------------------------------------
set JDK_HOME=f:\jbuilder4\jdk1.3

REM --------------------------------------------------------------------------
REM Do not modify anything below this line.
REM --------------------------------------------------------------------------
set JAVAC=%JDK_HOME%\bin\javac
set JAR=%JDK_HOME%\bin\jar
md lib
%JAVAC% -d lib galaxy\io\*.java galaxy\lang\*.java galaxy\server\*.java galaxy\server\ui\*.java galaxy\util\*.java

copy galaxy\server\ui\*.gif lib\galaxy\server\ui
cd lib
%JAR% cvf galaxy.jar galaxy
cd ..
