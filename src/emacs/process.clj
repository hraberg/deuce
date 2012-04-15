(ns emacs.process (use [deuce.core]) (require [clojure.core :as core]) (:refer-clojure :only []))

(defun delete-process (process)
  "Delete PROCESS: kill it and forget about it immediately.\nPROCESS may be a process, a buffer, the name of a process or buffer, or\n"
  )

(defun set-process-sentinel (process sentinel)
  "Give PROCESS the sentinel SENTINEL; nil for none.\nThe sentinel is called as a function when the process changes state.\n"
  )

(defun continue-process (&optional process current-group)
  "Continue process PROCESS.  May be process or name of one.\nSee function `interrupt-process' for more details on usage.\nIf PROCESS is a network or serial process, resume handling of incoming\n"
  )

(defun process-exit-status (process)
  "Return the exit status of PROCESS or the signal number that killed it.\n"
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
  "Send QUIT signal to process PROCESS.  May be process or name of one.\n"
  )

(defun process-tty-name (process)
  "Return the name of the terminal PROCESS uses, or nil if none.\nThis is the terminal that the process itself reads and writes on,\n"
  )

(defun stop-process (&optional process current-group)
  "Stop process PROCESS.  May be process or name of one.\nSee function `interrupt-process' for more details on usage.\nIf PROCESS is a network or serial process, inhibit handling of incoming\n"
  )

(defun process-send-string (process string)
  "Send PROCESS the contents of STRING as input.\nPROCESS may be a process, a buffer, the name of a process or buffer, or\nnil, indicating the current buffer's process.\nIf STRING is more than 500 characters long,\nit is sent in several bunches.  This may happen even for shorter strings.\n"
  )

(defun format-network-address (address &optional omit-port)
  "Convert network ADDRESS from internal format to a string.\nA 4 or 5 element vector represents an IPv4 address (with port number).\nAn 8 or 9 element vector represents an IPv6 address (with port number).\nIf optional second argument OMIT-PORT is non-nil, don't include a port\nnumber in the string, even when present in ADDRESS.\n"
  )

(defun network-interface-info (ifname)
  "Return information about network interface named IFNAME.\nThe return value is a list (ADDR BCAST NETMASK HWADDR FLAGS),\nwhere ADDR is the layer 3 address, BCAST is the layer 3 broadcast address,\nNETMASK is the layer 3 network mask, HWADDR is the layer 2 addres, and\n"
  )

(defun process-filter (process)
  "Returns the filter function of PROCESS; nil if none.\n"
  )

(defun process-filter-multibyte-p (process)
  "This function is obsolete since 23.1."
  )

(defun process-coding-system (process)
  )

(defun process-name (process)
  "Return the name of PROCESS, as a string.\nThis is the name of the program invoked in PROCESS,\n"
  )

(defun set-process-buffer (process buffer)
  )

(defun waiting-for-user-input-p ()
  "Returns non-nil if Emacs is waiting for input from the user.\n"
  )

(defun process-contact (process &optional key)
  "Return the contact info of PROCESS; t for a real child.\nFor a network or serial connection, the value depends on the optional\nKEY arg.  If KEY is nil, value is a cons cell of the form (HOST\nSERVICE) for a network connection or (PORT SPEED) for a serial\nconnection.  If KEY is t, the complete contact information for the\nconnection is returned, else the specific value for the keyword KEY is\nreturned.  See `make-network-process' or `make-serial-process' for a\n"
  )

(defun process-sentinel (process)
  "Return the sentinel of PROCESS; nil if none.\n"
  )

(defun process-send-region (process start end)
  "Send current contents of region as input to PROCESS.\nPROCESS may be a process, a buffer, the name of a process or buffer, or\nnil, indicating the current buffer's process.\nCalled from program, takes three arguments, PROCESS, START and END.\nIf the region is more than 500 characters long,\nit is sent in several bunches.  This may happen even for shorter regions.\n"
  )

(defun process-datagram-address (process)
  )

(defun process-status (process)
  "Return the status of PROCESS.\nThe returned value is one of the following symbols:\nrun  -- for a process that is running.\nstop -- for a process stopped but continuable.\nexit -- for a process that has exited.\nsignal -- for a process that has got a fatal signal.\nopen -- for a network stream connection that is open.\nlisten -- for a network stream server that is listening.\nclosed -- for a network stream connection that is closed.\nconnect -- when waiting for a non-blocking connection to complete.\nfailed -- when a non-blocking connection has failed.\nnil -- if arg is a process name and no such process exists.\nPROCESS may be a process, a buffer, the name of a process, or\n"
  )

(defun set-network-process-option (process option value &optional no-error)
  "For network process PROCESS set option OPTION to value VALUE.\nSee `make-network-process' for a list of options and values.\nIf optional fourth arg NO-ERROR is non-nil, don't signal an error if\n"
  )

(defun process-running-child-p (&optional process)
  "Return t if PROCESS has given the terminal to a child.\nIf the operating system does not make it possible to find out,\n"
  )

(defun process-buffer (process)
  "Return the buffer PROCESS is associated with.\n"
  )

(defun get-process (name)
  )

(defun set-process-filter (process filter)
  "Give PROCESS the filter function FILTER; nil means no filter.\nA value of t means stop accepting output from the process."
  )

(defun processp (object)
  )

(defun list-system-processes ()
  "Return a list of numerical process IDs of all running processes.\nIf this functionality is unsupported, return nil."
  )

(defun process-list ()
  )

(defun network-interface-list ()
  "Return an alist of all network interfaces and their network address.\nEach element is a cons, the car of which is a string containing the\ninterface name, and the cdr is the network address in internal\n"
  )

(defun process-plist (process)
  )

(defun process-type (process)
  "Return the connection type of PROCESS.\nThe value is either the symbol `real', `network', or `serial'.\nPROCESS may be a process, a buffer, the name of a process or buffer, or\n"
  )

(defun process-mark (process)
  )

(defun process-command (process)
  "Return the command that was executed to start PROCESS.\nThis is a list of strings, the first string being the program executed\nand the rest of the strings being the arguments given to it.\nFor a network or serial process, this is nil (process is running) or t\n"
  )

(defun kill-process (&optional process current-group)
  "Kill process PROCESS.  May be process or name of one.\nSee function `interrupt-process' for more details on usage.internal-describe-syntax-value is a built-in function in `C source\ncode'."
  )

(defun set-process-coding-system (process &optional decoding encoding)
  "Set coding systems of PROCESS to DECODING and ENCODING.\nDECODING will be used to decode subprocess output and ENCODING to\n"
  )

(defun make-serial-process (&rest args)
  "Create and return a serial port process."
  )

(defun serial-process-configure (&rest args)
  "Configure speed, bytesize, etc. of a serial process."
  )

(defun process-send-eof (&optional process)
  "Make PROCESS see end-of-file in its input.\nEOF comes after any text already sent to it.\nPROCESS may be a process, a buffer, the name of a process or buffer, or\nnil, indicating the current buffer's process.\nIf PROCESS is a network connection, or is a process communicating\nthrough a pipe (as opposed to a pty), then you cannot send any more\ntext to PROCESS after you call this function.\nIf PROCESS is a serial process, wait until all output written to the\n"
  )

(defun process-query-on-exit-flag (process)
  )

(defun get-buffer-process (buffer)
  "Return the (or a) process associated with BUFFER.\n"
  )

(defun process-id (process)
  "Return the process id of PROCESS.\nThis is the pid of the external process which PROCESS uses or talks to.\n"
  )

(defun accept-process-output (&optional process seconds millisec just-this-one)
  "Allow any pending output from subprocesses to be read by Emacs.\nIt is read into the process' buffers or given to their filter functions.\nNon-nil arg PROCESS means do not return until some output has been received\nfrom PROCESS."
  )

(defun start-process (name buffer program &rest program-args)
  "Start a program in a subprocess.  Return the process object for it.\nNAME is name for process.  It is modified if necessary to make it unique.\nBUFFER is the buffer (or buffer name) to associate with the process."
  )

(defun set-process-plist (process plist)
  "Replace the plist of PROCESS with PLIST.  Returns PLIST.defining-kbd-macro is an interactive built-in function in `C source\ncode'."
  )

(defun interrupt-process (&optional process current-group)
  "Interrupt process PROCESS.\nPROCESS may be a process, a buffer, or the name of a process or buffer.\nNo arg or nil means current buffer's process.\nSecond arg CURRENT-GROUP non-nil means send signal to\nthe current process-group of the process's controlling terminal\nrather than to the process's own process group.\nIf the process is a shell, this means interrupt current subjob\nrather than the shell."
  )
