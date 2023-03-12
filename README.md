
# Digistar-mode

This is an Emacs major-mode for editing Digistar scripts.

## Commands

### C-c TAB digistar-insert-filepath

Browse for a file and insert that filepath, after unresolving it to a Digistar path.

### C-c C-l digistar-show-lis-file

Show the .lis file that corresponds to the current Digistar script file, if it exists.

### C-c C-p digistar-play-script

Play this script in Digistar. If region is active, write its contents to a temporary file, and play that script in Digistar. If the buffer is narrowed, play only that portion.

The contents of the generated LIS file will be shown in the *Digistar Lis* buffer in a non-selected window, and if a temporary file was created, both the temporary file and its associated LIS file will be automatically deleted.

When playing a region, relative paths will be resolved.

### C-c C-r digistar-time-record-mode

Digistar-Time-Record mode is a minor mode that records timestamps into a Digistar script in realtime when you press SPC or S-SPC.  Once enabled, the first press of SPC initializes the relative clock to `digistar-absolute-time-at-point`.  Subsequent presses of SPC or S-SPC insert new timestamps into the script based on that initialization time.  SPC inserts an absolute timestamp and S-SPC inserts a relative timestamp.

### C-c C-t digistar-show-absolute-time

Show absolute time (in-script) of the current line.  If mark is active, the duration between point and mark will be reported instead.  With prefix argument, inserts the result.

# Digistar-MRSLog-mode

This is an Emacs major-mode for reading Digistar mrslog files.

It currently provides only basic highlighting.
