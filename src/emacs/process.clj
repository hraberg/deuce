(ns emacs.process (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun delete-process (process)
  "Delete PROCESS: kill it and forget about it immediately.
  PROCESS may be a process, a buffer, the name of a process or buffer, or
  nil, indicating the current buffer's process."
  )

(defun signal-process (process sigcode)
  "Send PROCESS the signal with code SIGCODE.
  PROCESS may also be a number specifying the process id of the
  process to signal; in this case, the process need not be a child of
  this Emacs.
  SIGCODE may be an integer, or a symbol whose name is a signal name."
  )

(defun set-process-sentinel (process sentinel)
  "Give PROCESS the sentinel SENTINEL; nil for none.
  The sentinel is called as a function when the process changes state.
  It gets two arguments: the process, and a string describing the change."
  )

(defun continue-process (&optional process current-group)
  "Continue process PROCESS.  May be process or name of one.
  See function `interrupt-process' for more details on usage.
  If PROCESS is a network or serial process, resume handling of incoming
  traffic."
  )

(defun process-exit-status (process)
  "Return the exit status of PROCESS or the signal number that killed it.
  If PROCESS has not yet exited or died, return 0."
  )

(defun set-process-window-size (process height width)
  "Tell PROCESS that it has logical window size HEIGHT and WIDTH."
  )

(defun process-attributes (pid)
  "Return attributes of the process given by its PID, a number."
  )

(defun make-network-process (&rest args)
  "Create and return a network server or client process."
  )

(defun set-process-query-on-exit-flag (process flag)
  "Specify if query is needed for PROCESS when Emacs is exited.
  If the second argument FLAG is non-nil, Emacs will query the user before
  exiting or killing a buffer if PROCESS is running."
  )

(defun quit-process (&optional process current-group)
  "Send QUIT signal to process PROCESS.  May be process or name of one.
  See function `interrupt-process' for more details on usage."
  )

(defun process-tty-name (process)
  "Return the name of the terminal PROCESS uses, or nil if none.
  This is the terminal that the process itself reads and writes on,
  not the name of the pty that Emacs uses to talk with that terminal."
  )

(defun stop-process (&optional process current-group)
  "Stop process PROCESS.  May be process or name of one.
  See function `interrupt-process' for more details on usage.
  If PROCESS is a network or serial process, inhibit handling of incoming
  traffic."
  )

(defun process-send-string (process string)
  "Send PROCESS the contents of STRING as input.
  PROCESS may be a process, a buffer, the name of a process or buffer, or
  nil, indicating the current buffer's process.
  If STRING is more than 500 characters long,
  it is sent in several bunches.  This may happen even for shorter strings.
  Output from processes can arrive in between bunches."
  )

(defun format-network-address (address &optional omit-port)
  "Convert network ADDRESS from internal format to a string.
  A 4 or 5 element vector represents an IPv4 address (with port number).
  An 8 or 9 element vector represents an IPv6 address (with port number).
  If optional second argument OMIT-PORT is non-nil, don't include a port
  number in the string, even when present in ADDRESS.
  Returns nil if format of ADDRESS is invalid."
  )

(defun network-interface-info (ifname)
  "Return information about network interface named IFNAME.
  The return value is a list (ADDR BCAST NETMASK HWADDR FLAGS),
  where ADDR is the layer 3 address, BCAST is the layer 3 broadcast address,
  NETMASK is the layer 3 network mask, HWADDR is the layer 2 addres, and
  FLAGS is the current flags of the interface."
  )

(defun process-filter (process)
  "Returns the filter function of PROCESS; nil if none.
  See `set-process-filter' for more info on filter functions."
  )

(defun process-inherit-coding-system-flag (process)
  "Return the value of inherit-coding-system flag for PROCESS.
  If this flag is t, `buffer-file-coding-system' of the buffer
  associated with PROCESS will inherit the coding system used to decode
  the process output."
  )

(defun process-filter-multibyte-p (process)
  "This function is obsolete since 23.1."
  )

(defun process-coding-system (process)
  "Return a cons of coding systems for decoding and encoding of PROCESS."
  )

(defun process-name (process)
  "Return the name of PROCESS, as a string.
  This is the name of the program invoked in PROCESS,
  possibly modified to make it unique among process names."
  )

(defun set-process-buffer (process buffer)
  "Set buffer associated with PROCESS to BUFFER (a buffer, or nil)."
  )

(defun waiting-for-user-input-p ()
  "Returns non-nil if Emacs is waiting for input from the user.
  This is intended for use by asynchronous process output filters and sentinels."
  )

(defun process-contact (process &optional key)
  "Return the contact info of PROCESS; t for a real child.
  For a network or serial connection, the value depends on the optional
  KEY arg.  If KEY is nil, value is a cons cell of the form (HOST
  SERVICE) for a network connection or (PORT SPEED) for a serial
  connection.  If KEY is t, the complete contact information for the
  connection is returned, else the specific value for the keyword KEY is
  returned.  See `make-network-process' or `make-serial-process' for a
  list of keywords."
  )

(defun process-sentinel (process)
  "Return the sentinel of PROCESS; nil if none.
  See `set-process-sentinel' for more info on sentinels."
  )

(defun process-send-region (process start end)
  "Send current contents of region as input to PROCESS.
  PROCESS may be a process, a buffer, the name of a process or buffer, or
  nil, indicating the current buffer's process.
  Called from program, takes three arguments, PROCESS, START and END.
  If the region is more than 500 characters long,
  it is sent in several bunches.  This may happen even for shorter regions.
  Output from processes can arrive in between bunches."
  )

(defun process-datagram-address (process)
  "Get the current datagram address associated with PROCESS."
  )

(defun process-status (process)
  "Return the status of PROCESS.
  The returned value is one of the following symbols:
  run  -- for a process that is running.
  stop -- for a process stopped but continuable.
  exit -- for a process that has exited.
  signal -- for a process that has got a fatal signal.
  open -- for a network stream connection that is open.
  listen -- for a network stream server that is listening.
  closed -- for a network stream connection that is closed.
  connect -- when waiting for a non-blocking connection to complete.
  failed -- when a non-blocking connection has failed.
  nil -- if arg is a process name and no such process exists.
  PROCESS may be a process, a buffer, the name of a process, or
  nil, indicating the current buffer's process."
  )

(defun set-network-process-option (process option value &optional no-error)
  "For network process PROCESS set option OPTION to value VALUE.
  See `make-network-process' for a list of options and values.
  If optional fourth arg NO-ERROR is non-nil, don't signal an error if
  OPTION is not a supported option, return nil instead; otherwise return t."
  )

(defun process-running-child-p (&optional process)
  "Return t if PROCESS has given the terminal to a child.
  If the operating system does not make it possible to find out,
  return t unconditionally."
  )

(defun process-buffer (process)
  "Return the buffer PROCESS is associated with.
  Output from PROCESS is inserted in this buffer unless PROCESS has a filter."
  )

(defun get-process (name)
  "Return the process named NAME, or nil if there is none."
  )

(defun set-process-filter (process filter)
  "Give PROCESS the filter function FILTER; nil means no filter.
  A value of t means stop accepting output from the process."
  )

(defun processp (object)
  "Return t if OBJECT is a process."
  )

(defun list-system-processes ()
  "Return a list of numerical process IDs of all running processes.
  If this functionality is unsupported, return nil."
  )

(defun set-process-inherit-coding-system-flag (process flag)
  "Determine whether buffer of PROCESS will inherit coding-system.
  If the second argument FLAG is non-nil, then the variable
  `buffer-file-coding-system' of the buffer associated with PROCESS
  will be bound to the value of the coding system used to decode
  the process output."
  )

(defun process-list ()
  "Return a list of all processes."
  )

(defun network-interface-list ()
  "Return an alist of all network interfaces and their network address.
  Each element is a cons, the car of which is a string containing the
  interface name, and the cdr is the network address in internal
  format; see the description of ADDRESS in `make-network-process'."
  )

(defun process-plist (process)
  "Return the plist of PROCESS."
  )

(defun process-type (process)
  "Return the connection type of PROCESS.
  The value is either the symbol `real', `network', or `serial'.
  PROCESS may be a process, a buffer, the name of a process or buffer, or
  nil, indicating the current buffer's process."
  )

(defun process-mark (process)
  "Return the marker for the end of the last output from PROCESS."
  )

(defun process-command (process)
  "Return the command that was executed to start PROCESS.
  This is a list of strings, the first string being the program executed
  and the rest of the strings being the arguments given to it.
  For a network or serial process, this is nil (process is running) or t
  (process is stopped)."
  )

(defun kill-process (&optional process current-group)
  "Kill process PROCESS.  May be process or name of one.
  See function `interrupt-process' for more details on usage."
  )

(defun list-processes (&optional query-only)
  "Display a list of all processes.
  If optional argument QUERY-ONLY is non-nil, only processes with
  the query-on-exit flag set will be listed.
  Any process listed as exited or signaled is actually eliminated
  after the listing is made."
  )

(defun set-process-coding-system (process &optional decoding encoding)
  "Set coding systems of PROCESS to DECODING and ENCODING.
  DECODING will be used to decode subprocess output and ENCODING to
  encode subprocess input."
  )

(defun make-serial-process (&rest args)
  "Create and return a serial port process."
  )

(defun serial-process-configure (&rest args)
  "Configure speed, bytesize, etc. of a serial process."
  )

(defun set-process-filter-multibyte (process flag)
  "This function is obsolete since 23.1."
  )

(defun process-send-eof (&optional process)
  "Make PROCESS see end-of-file in its input.
  EOF comes after any text already sent to it.
  PROCESS may be a process, a buffer, the name of a process or buffer, or
  nil, indicating the current buffer's process.
  If PROCESS is a network connection, or is a process communicating
  through a pipe (as opposed to a pty), then you cannot send any more
  text to PROCESS after you call this function.
  If PROCESS is a serial process, wait until all output written to the
  process has been transmitted to the serial port."
  )

(defun set-process-datagram-address (process address)
  "Set the datagram address for PROCESS to ADDRESS.
  Returns nil upon error setting address, ADDRESS otherwise."
  )

(defun process-query-on-exit-flag (process)
  "Return the current value of query-on-exit flag for PROCESS."
  )

(defun get-buffer-process (buffer)
  "Return the (or a) process associated with BUFFER.
  BUFFER may be a buffer or the name of one."
  )

(defun process-id (process)
  "Return the process id of PROCESS.
  This is the pid of the external process which PROCESS uses or talks to.
  For a network connection, this value is nil."
  )

(defun accept-process-output (&optional process seconds millisec just-this-one)
  "Allow any pending output from subprocesses to be read by Emacs.
  It is read into the process' buffers or given to their filter functions.
  Non-nil arg PROCESS means do not return until some output has been received
  from PROCESS."
  )

(defun start-process (name buffer program &rest program-args)
  "Start a program in a subprocess.  Return the process object for it.
  NAME is name for process.  It is modified if necessary to make it unique.
  BUFFER is the buffer (or buffer name) to associate with the process."
  )

(defun set-process-plist (process plist)
  "Replace the plist of PROCESS with PLIST.  Returns PLIST."
  )

(defun interrupt-process (&optional process current-group)
  "Interrupt process PROCESS.
  PROCESS may be a process, a buffer, or the name of a process or buffer.
  No arg or nil means current buffer's process.
  Second arg CURRENT-GROUP non-nil means send signal to
  the current process-group of the process's controlling terminal
  rather than to the process's own process group.
  If the process is a shell, this means interrupt current subjob
  rather than the shell."
  )
