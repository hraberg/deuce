(ns
 deuce.emacs.process
 (:use [deuce.emacs-lisp :only (defun defvar)])
 (:refer-clojure :exclude []))

(defvar delete-exited-processes nil
  "*Non-nil means delete processes immediately when they exit.
  A value of nil means don't delete them until `list-processes' is run.
  
  You can customize this variable.")

(defvar process-connection-type nil
  "Control type of device used to communicate with subprocesses.
  Values are nil to use a pipe, or t or `pty' to use a pty.
  The value has no effect if the system has no ptys or if all ptys are busy:
  then a pipe is used in any case.
  The value takes effect when `start-process' is called.")

(defvar process-adaptive-read-buffering nil
  "If non-nil, improve receive buffering by delaying after short reads.
  On some systems, when Emacs reads the output from a subprocess, the output data
  is read in very small blocks, potentially resulting in very poor performance.
  This behavior can be remedied to some extent by setting this variable to a
  non-nil value, as it will automatically delay reading from such processes, to
  allow them to produce more output before Emacs tries to read it.
  If the value is t, the delay is reset after each write to the process; any other
  non-nil value means that the delay is not reset on write.
  The variable takes effect when `start-process' is called.")

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
  "Return attributes of the process given by its PID, a number.
  
  Value is an alist where each element is a cons cell of the form
  
      (KEY . VALUE)
  
  If this functionality is unsupported, the value is nil.
  
  See `list-system-processes' for getting a list of all process IDs.
  
  The KEYs of the attributes that this function may return are listed
  below, together with the type of the associated VALUE (in parentheses).
  Not all platforms support all of these attributes; unsupported
  attributes will not appear in the returned alist.
  Unless explicitly indicated otherwise, numbers can have either
  integer or floating point values.
  
   euid    -- Effective user User ID of the process (number)
   user    -- User name corresponding to euid (string)
   egid    -- Effective user Group ID of the process (number)
   group   -- Group name corresponding to egid (string)
   comm    -- Command name (executable name only) (string)
   state   -- Process state code, such as \"S\", \"R\", or \"T\" (string)
   ppid    -- Parent process ID (number)
   pgrp    -- Process group ID (number)
   sess    -- Session ID, i.e. process ID of session leader (number)
   ttname  -- Controlling tty name (string)
   tpgid   -- ID of foreground process group on the process's tty (number)
   minflt  -- number of minor page faults (number)
   majflt  -- number of major page faults (number)
   cminflt -- cumulative number of minor page faults (number)
   cmajflt -- cumulative number of major page faults (number)
   utime   -- user time used by the process, in the (HIGH LOW USEC) format
   stime   -- system time used by the process, in the (HIGH LOW USEC) format
   time    -- sum of utime and stime, in the (HIGH LOW USEC) format
   cutime  -- user time used by the process and its children, (HIGH LOW USEC)
   cstime  -- system time used by the process and its children, (HIGH LOW USEC)
   ctime   -- sum of cutime and cstime, in the (HIGH LOW USEC) format
   pri     -- priority of the process (number)
   nice    -- nice value of the process (number)
   thcount -- process thread count (number)
   start   -- time the process started, in the (HIGH LOW USEC) format
   vsize   -- virtual memory size of the process in KB's (number)
   rss     -- resident set size of the process in KB's (number)
   etime   -- elapsed time the process is running, in (HIGH LOW USEC) format
   pcpu    -- percents of CPU time used by the process (floating-point number)
   pmem    -- percents of total physical memory used by process's resident set
                (floating-point number)
   args    -- command line which invoked the process (string)."
  )

(defun make-network-process (&rest args)
  "Create and return a network server or client process.
  
  In Emacs, network connections are represented by process objects, so
  input and output work as for subprocesses and `delete-process' closes
  a network connection.  However, a network process has no process id,
  it cannot be signaled, and the status codes are different from normal
  processes.
  
  Arguments are specified as keyword/argument pairs.  The following
  arguments are defined:
  
  :name NAME -- NAME is name for process.  It is modified if necessary
  to make it unique.
  
  :buffer BUFFER -- BUFFER is the buffer (or buffer-name) to associate
  with the process.  Process output goes at end of that buffer, unless
  you specify an output stream or filter function to handle the output.
  BUFFER may be also nil, meaning that this process is not associated
  with any buffer.
  
  :host HOST -- HOST is name of the host to connect to, or its IP
  address.  The symbol `local' specifies the local host.  If specified
  for a server process, it must be a valid name or address for the local
  host, and only clients connecting to that address will be accepted.
  
  :service SERVICE -- SERVICE is name of the service desired, or an
  integer specifying a port number to connect to.  If SERVICE is t,
  a random port number is selected for the server.  (If Emacs was
  compiled with getaddrinfo, a port number can also be specified as a
  string, e.g. \"80\", as well as an integer.  This is not portable.)
  
  :type TYPE -- TYPE is the type of connection.  The default (nil) is a
  stream type connection, `datagram' creates a datagram type connection,
  `seqpacket' creates a reliable datagram connection.
  
  :family FAMILY -- FAMILY is the address (and protocol) family for the
  service specified by HOST and SERVICE.  The default (nil) is to use
  whatever address family (IPv4 or IPv6) that is defined for the host
  and port number specified by HOST and SERVICE.  Other address families
  supported are:
    local -- for a local (i.e. UNIX) address specified by SERVICE.
    ipv4  -- use IPv4 address family only.
    ipv6  -- use IPv6 address family only.
  
  :local ADDRESS -- ADDRESS is the local address used for the connection.
  This parameter is ignored when opening a client process. When specified
  for a server process, the FAMILY, HOST and SERVICE args are ignored.
  
  :remote ADDRESS -- ADDRESS is the remote partner's address for the
  connection.  This parameter is ignored when opening a stream server
  process.  For a datagram server process, it specifies the initial
  setting of the remote datagram address.  When specified for a client
  process, the FAMILY, HOST, and SERVICE args are ignored.
  
  The format of ADDRESS depends on the address family:
  - An IPv4 address is represented as an vector of integers [A B C D P]
  corresponding to numeric IP address A.B.C.D and port number P.
  - A local address is represented as a string with the address in the
  local address space.
  - An \"unsupported family\" address is represented by a cons (F . AV)
  where F is the family number and AV is a vector containing the socket
  address data with one element per address data byte.  Do not rely on
  this format in portable code, as it may depend on implementation
  defined constants, data sizes, and data structure alignment.
  
  :coding CODING -- If CODING is a symbol, it specifies the coding
  system used for both reading and writing for this process.  If CODING
  is a cons (DECODING . ENCODING), DECODING is used for reading, and
  ENCODING is used for writing.
  
  :nowait BOOL -- If BOOL is non-nil for a stream type client process,
  return without waiting for the connection to complete; instead, the
  sentinel function will be called with second arg matching \"open\" (if
  successful) or \"failed\" when the connect completes.  Default is to use
  a blocking connect (i.e. wait) for stream type connections.
  
  :noquery BOOL -- Query the user unless BOOL is non-nil, and process is
  running when Emacs is exited.
  
  :stop BOOL -- Start process in the `stopped' state if BOOL non-nil.
  In the stopped state, a server process does not accept new
  connections, and a client process does not handle incoming traffic.
  The stopped state is cleared by `continue-process' and set by
  `stop-process'.
  
  :filter FILTER -- Install FILTER as the process filter.
  
  :filter-multibyte BOOL -- If BOOL is non-nil, strings given to the
  process filter are multibyte, otherwise they are unibyte.
  If this keyword is not specified, the strings are multibyte if
  the default value of `enable-multibyte-characters' is non-nil.
  
  :sentinel SENTINEL -- Install SENTINEL as the process sentinel.
  
  :log LOG -- Install LOG as the server process log function.  This
  function is called when the server accepts a network connection from a
  client.  The arguments are SERVER, CLIENT, and MESSAGE, where SERVER
  is the server process, CLIENT is the new process for the connection,
  and MESSAGE is a string.
  
  :plist PLIST -- Install PLIST as the new process' initial plist.
  
  :server QLEN -- if QLEN is non-nil, create a server process for the
  specified FAMILY, SERVICE, and connection type (stream or datagram).
  If QLEN is an integer, it is used as the max. length of the server's
  pending connection queue (also known as the backlog); the default
  queue length is 5.  Default is to create a client process.
  
  The following network options can be specified for this connection:
  
  :broadcast BOOL    -- Allow send and receive of datagram broadcasts.
  :dontroute BOOL    -- Only send to directly connected hosts.
  :keepalive BOOL    -- Send keep-alive messages on network stream.
  :linger BOOL or TIMEOUT -- Send queued messages before closing.
  :oobinline BOOL    -- Place out-of-band data in receive data stream.
  :priority INT      -- Set protocol defined priority for sent packets.
  :reuseaddr BOOL    -- Allow reusing a recently used local address
                        (this is allowed by default for a server process).
  :bindtodevice NAME -- bind to interface NAME.  Using this may require
                        special privileges on some systems.
  
  Consult the relevant system programmer's manual pages for more
  information on using these options.
  
  
  A server process will listen for and accept connections from clients.
  When a client connection is accepted, a new network process is created
  for the connection with the following parameters:
  
  - The client's process name is constructed by concatenating the server
  process' NAME and a client identification string.
  - If the FILTER argument is non-nil, the client process will not get a
  separate process buffer; otherwise, the client's process buffer is a newly
  created buffer named after the server process' BUFFER name or process
  NAME concatenated with the client identification string.
  - The connection type and the process filter and sentinel parameters are
  inherited from the server process' TYPE, FILTER and SENTINEL.
  - The client process' contact info is set according to the client's
  addressing information (typically an IP address and a port number).
  - The client process' plist is initialized from the server's plist.
  
  Notice that the FILTER and SENTINEL args are never used directly by
  the server process.  Also, the BUFFER argument is not used directly by
  the server process, but via the optional :log function, accepted (and
  failed) connections may be logged in the server process' buffer.
  
  The original argument list, modified with the actual connection
  information, is available via the `process-contact' function."
  )

(defun set-process-query-on-exit-flag (process flag)
  "Specify if query is needed for PROCESS when Emacs is exited.
  If the second argument FLAG is non-nil, Emacs will query the user before
  exiting or killing a buffer if PROCESS is running.  This function
  returns FLAG."
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
  NETMASK is the layer 3 network mask, HWADDR is the layer 2 address, and
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
  "This function is obsolete since 23.1.
  
  Return t if a multibyte string is given to PROCESS's filter."
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
  "Set buffer associated with PROCESS to BUFFER (a buffer, or nil).
  Return BUFFER."
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
  A value of t means stop accepting output from the process.
  
  When a process has a filter, its buffer is not used for output.
  Instead, each time it does output, the entire string of output is
  passed to the filter.
  
  The filter gets two arguments: the process and the string of output.
  The string argument is normally a multibyte string, except:
  - if the process' input coding system is no-conversion or raw-text,
    it is a unibyte string (the non-converted input), or else
  - if `default-enable-multibyte-characters' is nil, it is a unibyte
    string (the result of converting the decoded input multibyte
    string to unibyte with `string-make-unibyte')."
  )

(defun processp (object)
  "Return t if OBJECT is a process."
  )

(defun list-system-processes ()
  "Return a list of numerical process IDs of all running processes.
  If this functionality is unsupported, return nil.
  
  See `process-attributes' for getting attributes of a process given its ID."
  )

(defun set-process-inherit-coding-system-flag (process flag)
  "Determine whether buffer of PROCESS will inherit coding-system.
  If the second argument FLAG is non-nil, then the variable
  `buffer-file-coding-system' of the buffer associated with PROCESS
  will be bound to the value of the coding system used to decode
  the process output.
  
  This is useful when the coding system specified for the process buffer
  leaves either the character code conversion or the end-of-line conversion
  unspecified, or if the coding system used to decode the process output
  is more appropriate for saving the process buffer.
  
  Binding the variable `inherit-process-coding-system' to non-nil before
  starting the process is an alternative way of setting the inherit flag
  for the process which will run.
  
  This function returns FLAG."
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

(defun set-process-coding-system (process &optional decoding encoding)
  "Set coding systems of PROCESS to DECODING and ENCODING.
  DECODING will be used to decode subprocess output and ENCODING to
  encode subprocess input."
  )

(defun make-serial-process (&rest args)
  "Create and return a serial port process.
  
  In Emacs, serial port connections are represented by process objects,
  so input and output work as for subprocesses, and `delete-process'
  closes a serial port connection.  However, a serial process has no
  process id, it cannot be signaled, and the status codes are different
  from normal processes.
  
  `make-serial-process' creates a process and a buffer, on which you
  probably want to use `process-send-string'.  Try M-x serial-term for
  an interactive terminal.  See below for examples.
  
  Arguments are specified as keyword/argument pairs.  The following
  arguments are defined:
  
  :port PORT -- (mandatory) PORT is the path or name of the serial port.
  For example, this could be \"/dev/ttyS0\" on Unix.  On Windows, this
  could be \"COM1\", or \"\\\\.\\COM10\" for ports higher than COM9 (double
  the backslashes in strings).
  
  :speed SPEED -- (mandatory) is handled by `serial-process-configure',
  which this function calls.
  
  :name NAME -- NAME is the name of the process.  If NAME is not given,
  the value of PORT is used.
  
  :buffer BUFFER -- BUFFER is the buffer (or buffer-name) to associate
  with the process.  Process output goes at the end of that buffer,
  unless you specify an output stream or filter function to handle the
  output.  If BUFFER is not given, the value of NAME is used.
  
  :coding CODING -- If CODING is a symbol, it specifies the coding
  system used for both reading and writing for this process.  If CODING
  is a cons (DECODING . ENCODING), DECODING is used for reading, and
  ENCODING is used for writing.
  
  :noquery BOOL -- When exiting Emacs, query the user if BOOL is nil and
  the process is running.  If BOOL is not given, query before exiting.
  
  :stop BOOL -- Start process in the `stopped' state if BOOL is non-nil.
  In the stopped state, a serial process does not accept incoming data,
  but you can send outgoing data.  The stopped state is cleared by
  `continue-process' and set by `stop-process'.
  
  :filter FILTER -- Install FILTER as the process filter.
  
  :sentinel SENTINEL -- Install SENTINEL as the process sentinel.
  
  :plist PLIST -- Install PLIST as the initial plist of the process.
  
  :bytesize
  :parity
  :stopbits
  :flowcontrol
  -- This function calls `serial-process-configure' to handle these
  arguments.
  
  The original argument list, possibly modified by later configuration,
  is available via the function `process-contact'.
  
  Examples:
  
  (make-serial-process :port \"/dev/ttyS0\" :speed 9600)
  
  (make-serial-process :port \"COM1\" :speed 115200 :stopbits 2)
  
  (make-serial-process :port \"\\\\.\\COM13\" :speed 1200 :bytesize 7 :parity 'odd)
  
  (make-serial-process :port \"/dev/tty.BlueConsole-SPP-1\" :speed nil)"
  )

(defun serial-process-configure (&rest args)
  "Configure speed, bytesize, etc. of a serial process.
  
  Arguments are specified as keyword/argument pairs.  Attributes that
  are not given are re-initialized from the process's current
  configuration (available via the function `process-contact') or set to
  reasonable default values.  The following arguments are defined:
  
  :process PROCESS
  :name NAME
  :buffer BUFFER
  :port PORT
  -- Any of these arguments can be given to identify the process that is
  to be configured.  If none of these arguments is given, the current
  buffer's process is used.
  
  :speed SPEED -- SPEED is the speed of the serial port in bits per
  second, also called baud rate.  Any value can be given for SPEED, but
  most serial ports work only at a few defined values between 1200 and
  115200, with 9600 being the most common value.  If SPEED is nil, the
  serial port is not configured any further, i.e., all other arguments
  are ignored.  This may be useful for special serial ports such as
  Bluetooth-to-serial converters which can only be configured through AT
  commands.  A value of nil for SPEED can be used only when passed
  through `make-serial-process' or `serial-term'.
  
  :bytesize BYTESIZE -- BYTESIZE is the number of bits per byte, which
  can be 7 or 8.  If BYTESIZE is not given or nil, a value of 8 is used.
  
  :parity PARITY -- PARITY can be nil (don't use parity), the symbol
  `odd' (use odd parity), or the symbol `even' (use even parity).  If
  PARITY is not given, no parity is used.
  
  :stopbits STOPBITS -- STOPBITS is the number of stopbits used to
  terminate a byte transmission.  STOPBITS can be 1 or 2.  If STOPBITS
  is not given or nil, 1 stopbit is used.
  
  :flowcontrol FLOWCONTROL -- FLOWCONTROL determines the type of
  flowcontrol to be used, which is either nil (don't use flowcontrol),
  the symbol `hw' (use RTS/CTS hardware flowcontrol), or the symbol `sw'
  (use XON/XOFF software flowcontrol).  If FLOWCONTROL is not given, no
  flowcontrol is used.
  
  `serial-process-configure' is called by `make-serial-process' for the
  initial configuration of the serial port.
  
  Examples:
  
  (serial-process-configure :process \"/dev/ttyS0\" :speed 1200)
  
  (serial-process-configure
      :buffer \"COM1\" :stopbits 1 :parity 'odd :flowcontrol 'hw)
  
  (serial-process-configure :port \"\\\\.\\COM13\" :bytesize 7)"
  )

(defun set-process-filter-multibyte (process flag)
  "This function is obsolete since 23.1.
  
  Set multibyteness of the strings given to PROCESS's filter.
  If FLAG is non-nil, the filter is given multibyte strings.
  If FLAG is nil, the filter is given unibyte strings.  In this case,
  all character code conversion except for end-of-line conversion is
  suppressed."
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
  from PROCESS.
  
  Non-nil second arg SECONDS and third arg MILLISEC are number of seconds
  and milliseconds to wait; return after that much time whether or not
  there is any subprocess output.  If SECONDS is a floating point number,
  it specifies a fractional number of seconds to wait.
  The MILLISEC argument is obsolete and should be avoided.
  
  If optional fourth arg JUST-THIS-ONE is non-nil, only accept output
  from PROCESS, suspending reading output from other processes.
  If JUST-THIS-ONE is an integer, don't run any timers either.
  Return non-nil if we received any output before the timeout expired."
  )

(defun start-process (name buffer program &rest program-args)
  "Start a program in a subprocess.  Return the process object for it.
  NAME is name for process.  It is modified if necessary to make it unique.
  BUFFER is the buffer (or buffer name) to associate with the process.
  
  Process output (both standard output and standard error streams) goes
  at end of BUFFER, unless you specify an output stream or filter
  function to handle the output.  BUFFER may also be nil, meaning that
  this process is not associated with any buffer.
  
  PROGRAM is the program file name.  It is searched for in `exec-path'
  (which see).  If nil, just associate a pty with the buffer.  Remaining
  arguments are strings to give program as arguments.
  
  If you want to separate standard output from standard error, invoke
  the command through a shell and redirect one of them using the shell
  syntax."
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
  rather than the shell.
  
  If CURRENT-GROUP is `lambda', and if the shell owns the terminal,
  don't send the signal."
  )
