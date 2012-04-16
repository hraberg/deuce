(ns emacs.process (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun delete-process (process)
  "Delete PROCESS: kill it and forget about it immediately.
  PROCESS may be a process, a buffer, the name of a process or buffer, or"
  )

(defun set-process-sentinel (process sentinel)
  "Give PROCESS the sentinel SENTINEL; nil for none.
  The sentinel is called as a function when the process changes state."
  )

(defun continue-process (&optional process current-group)
  "Continue process PROCESS.  May be process or name of one.
  See function `interrupt-process' for more details on usage.
  If PROCESS is a network or serial process, resume handling of incoming"
  )

(defun process-exit-status (process)
  "Return the exit status of PROCESS or the signal number that killed it."
  )

(defun set-process-window-size (process height width)
  )

(defun process-attributes (pid)
  "Return attributes of the process given by its PID, a number."
  )

(defun make-network-process (&rest args)
  "Create and return a network server or client process."
  )

(defun quit-process (&optional process current-group)
  "Send QUIT signal to process PROCESS.  May be process or name of one."
  )

(defun process-tty-name (process)
  "Return the name of the terminal PROCESS uses, or nil if none.
  This is the terminal that the process itself reads and writes on,"
  )

(defun stop-process (&optional process current-group)
  "Stop process PROCESS.  May be process or name of one.
  See function `interrupt-process' for more details on usage.
  If PROCESS is a network or serial process, inhibit handling of incoming"
  )

(defun process-send-string (process string)
  "Send PROCESS the contents of STRING as input.
  PROCESS may be a process, a buffer, the name of a process or buffer, or
  nil, indicating the current buffer's process.
  If STRING is more than 500 characters long,
  it is sent in several bunches.  This may happen even for shorter strings."
  )

(defun format-network-address (address &optional omit-port)
  "Convert network ADDRESS from internal format to a string.
  A 4 or 5 element vector represents an IPv4 address (with port number).
  An 8 or 9 element vector represents an IPv6 address (with port number).
  If optional second argument OMIT-PORT is non-nil, don't include a port
  number in the string, even when present in ADDRESS."
  )

(defun network-interface-info (ifname)
  "Return information about network interface named IFNAME.
  The return value is a list (ADDR BCAST NETMASK HWADDR FLAGS),
  where ADDR is the layer 3 address, BCAST is the layer 3 broadcast address,
  NETMASK is the layer 3 network mask, HWADDR is the layer 2 addres, and"
  )

(defun process-filter (process)
  "Returns the filter function of PROCESS; nil if none."
  )

(defun process-filter-multibyte-p (process)
  "This function is obsolete since 23.1."
  )

(defun process-coding-system (process)
  )

(defun process-name (process)
  "Return the name of PROCESS, as a string.
  This is the name of the program invoked in PROCESS,"
  )

(defun set-process-buffer (process buffer)
  )

(defun waiting-for-user-input-p ()
  "Returns non-nil if Emacs is waiting for input from the user."
  )

(defun process-contact (process &optional key)
  "Return the contact info of PROCESS; t for a real child.
  For a network or serial connection, the value depends on the optional
  KEY arg.  If KEY is nil, value is a cons cell of the form (HOST
  SERVICE) for a network connection or (PORT SPEED) for a serial
  connection.  If KEY is t, the complete contact information for the
  connection is returned, else the specific value for the keyword KEY is
  returned.  See `make-network-process' or `make-serial-process' for a"
  )

(defun process-sentinel (process)
  "Return the sentinel of PROCESS; nil if none."
  )

(defun process-send-region (process start end)
  "Send current contents of region as input to PROCESS.
  PROCESS may be a process, a buffer, the name of a process or buffer, or
  nil, indicating the current buffer's process.
  Called from program, takes three arguments, PROCESS, START and END.
  If the region is more than 500 characters long,
  it is sent in several bunches.  This may happen even for shorter regions."
  )

(defun process-datagram-address (process)
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
  PROCESS may be a process, a buffer, the name of a process, or"
  )

(defun set-network-process-option (process option value &optional no-error)
  "For network process PROCESS set option OPTION to value VALUE.
  See `make-network-process' for a list of options and values.
  If optional fourth arg NO-ERROR is non-nil, don't signal an error if"
  )

(defun process-running-child-p (&optional process)
  "Return t if PROCESS has given the terminal to a child.
  If the operating system does not make it possible to find out,"
  )

(defun process-buffer (process)
  "Return the buffer PROCESS is associated with."
  )

(defun get-process (name)
  )

(defun set-process-filter (process filter)
  "Give PROCESS the filter function FILTER; nil means no filter.
  A value of t means stop accepting output from the process."
  )

(defun processp (object)
  )

(defun list-system-processes ()
  "Return a list of numerical process IDs of all running processes.
  If this functionality is unsupported, return nil."
  )

(defun process-list ()
  )

(defun network-interface-list ()
  "Return an alist of all network interfaces and their network address.
  Each element is a cons, the car of which is a string containing the
  interface name, and the cdr is the network address in internal"
  )

(defun process-plist (process)
  )

(defun process-type (process)
  "Return the connection type of PROCESS.
  The value is either the symbol `real', `network', or `serial'.
  PROCESS may be a process, a buffer, the name of a process or buffer, or"
  )

(defun process-mark (process)
  )

(defun process-command (process)
  "Return the command that was executed to start PROCESS.
  This is a list of strings, the first string being the program executed
  and the rest of the strings being the arguments given to it.
  For a network or serial process, this is nil (process is running) or t"
  )

(defun kill-process (&optional process current-group)
  "Kill process PROCESS.  May be process or name of one.
  See function `interrupt-process' for more details on usage.internal-describe-syntax-value is a built-in function in `C source
  code'."
  )

(defun set-process-coding-system (process &optional decoding encoding)
  "Set coding systems of PROCESS to DECODING and ENCODING.
  DECODING will be used to decode subprocess output and ENCODING to"
  )

(defun make-serial-process (&rest args)
  "Create and return a serial port process."
  )

(defun serial-process-configure (&rest args)
  "Configure speed, bytesize, etc. of a serial process."
  )

(defun process-send-eof (&optional process)
  "Make PROCESS see end-of-file in its input.
  EOF comes after any text already sent to it.
  PROCESS may be a process, a buffer, the name of a process or buffer, or
  nil, indicating the current buffer's process.
  If PROCESS is a network connection, or is a process communicating
  through a pipe (as opposed to a pty), then you cannot send any more
  text to PROCESS after you call this function.
  If PROCESS is a serial process, wait until all output written to the"
  )

(defun process-query-on-exit-flag (process)
  )

(defun get-buffer-process (buffer)
  "Return the (or a) process associated with BUFFER."
  )

(defun process-id (process)
  "Return the process id of PROCESS.
  This is the pid of the external process which PROCESS uses or talks to."
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
  "Replace the plist of PROCESS with PLIST.  Returns PLIST.defining-kbd-macro is an interactive built-in function in `C source
  code'."
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
