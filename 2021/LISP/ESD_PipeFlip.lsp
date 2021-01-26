(defun C:Pipe_Flip ()
(command "-vbarun"
"FlipPipe.dvb!ExcelFunctions.FlipPipe")
(princ)
)