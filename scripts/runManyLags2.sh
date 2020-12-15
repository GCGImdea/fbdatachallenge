
COUNTRIESTODO='"EC", "ES", "IT"' #, "CL", "FR"'
#COUNTRIESTODO='"PT"'
for PEN in TRUE FALSE; do
    for ALPHA in 0.5; do
	for RMCL in TRUE FALSE; do
	    for RMTH in 0.9; do
		for MILAG in 7 14; do
		    for MXLAG in 60 ; do
			for SIGTOMATCH in cases ; do
			    for SIGTOTRY in signals_umd ; do #signals_umd,signals_ccfr signals_umd,signals_ccfr,signals_nsum    #for SIGTOTRY in signals_umd;  do 
				for FIRSTCUTOFF in 2020-9-10; do
				    for LASTCUTOFF in 2020-11-10; do
					for CUTOFFINTERVAL in 1 ; do
					    for SMOOTH in FALSE ; do 
						for BASISDIM in 15 ; do
						    sed "s/#PEN#/$PEN/g" runlag.R | sed  "s/#ALPHA#/$ALPHA/g" | sed  "s/#RMCL#/$RMCL/g" | sed  "s/#RMTH#/$RMTH/g" | sed  "s/#MILAG#/$MILAG/g"| sed  "s/#MXLAG#/$MXLAG/g"| sed  "s/#SIGTOMATCH#/$SIGTOMATCH/g"    | sed  "s/#SIGTOTRY#/$SIGTOTRY/g" | sed  "s/#FIRSTCUTOFF#/$FIRSTCUTOFF/g"| sed  "s/#LASTCUTOFF#/$LASTCUTOFF/g" | sed  "s/#CUTOFFINTERVAL#/$CUTOFFINTERVAL/g"| sed  "s/#COUNTRIESTODO#/$COUNTRIESTODO/g" | sed "s/#SMOOTH#/$SMOOTH/g" |sed "s/#BASISDIM#/$BASISDIM/g" > runlag-pen$PEN$ALPHA-rmcl$RMCL$RMTH-smooth$SMOOTH$BASISDIM-lag$MILAG-$MXLAG-$SIGTOMATCH-$SIGTOTRY-$FIRSTCUTOFF-$LASTCUTOFF-$CUTOFFINTERVAL.R
						    Rscript runlag-pen$PEN$ALPHA-rmcl$RMCL$RMTH-smooth$SMOOTH$BASISDIM-lag$MILAG-$MXLAG-$SIGTOMATCH-$SIGTOTRY-$FIRSTCUTOFF-$LASTCUTOFF-$CUTOFFINTERVAL.R  > runlag-pen$PEN$ALPHA-rmcl$RMCL$RMTH-smooth$SMOOTH$BASISDIM-lag$MILAG-$MXLAG-$SIGTOMATCH-$SIGTOTRY-$FIRSTCUTOFF-$LASTCUTOFF-$CUTOFFINTERVAL.Rout 2>&1 &
						done
					    done 
					done
				    done
				done
			    done
			done
		    done
		done
	    done
	done
    done
done

					    
					   
#PEN# 
#ALPHA#
#RMCL# 
#RMTH# 

#MILAG#
#MXLAG#
#SIGTOMATCH#
#SIGTOTRY#

#FIRSTCUTOFF#
#LASTCUTOFF#
#CUTOFFINTERVAL#


