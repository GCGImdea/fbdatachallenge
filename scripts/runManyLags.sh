

for PEN in TRUE FALSE; do
    for ALPHA in 0.5; do
	for RMCL in TRUE FALSE; do
	    for RMTH in 0.9; do
		for MILAG in 7 ; do
		    for MXLAG in 60 ; do
			for SIGTOMATCH in cases ; do
			    #			    for SIGTOTRY in signals_umd "c(signals_umd,signals_ccfr)" "c(signals_umd,signals_ccfr,signals_nsum)"; do
			    for SIGTOTRY in signals_umd;  do 
				for FIRSTCUTOFF in 2020-9-10; do
				    for LASTCUTOFF in 2020-11-10; do
					for CUTOFFINTEVAL in 1 ; do

					    sed 's,#PEN#,$PEN' runLag.sh | sed -i bak 's,#ALPHA#,$ALPHA' | sed -i bak 's,#RMCL#,$RMCL' | sed -i bak 's,#RMTH#,$RMTH' | sed -i bak 's,#MILAG#,$MILAG'| sed -i bak 's,#MXLAG#,$MXLAG'| sed -i bak 's,#SIGTOMATCH#,$SIGTOMATCH'    | sed -i bak 's,#SIGTOTRY#,$SIGTOTRY' | sed -i bak 's,#FIRSTCUTOFF#,$FIRSTCUTOFF'| sed -i bak 's,#LASTCUTOFF#,$LASTCUTOFF' | sed -i bak 's,#CUTOFFINTERVAL#,$CUTOFFINTERVAL' > runlag-pen$PEN$ALPHA-rmcl$RMCL$RMTH-lag$MILAG-$MXLAG-$SIGTOMATCH-$SIGTOTRY-$FIRSTCUTOFF-$LASTCUTOFF-$CUTOFFINTERVAL.R
					    R CMD BATCH runlag-pen$PEN$ALPHA-rmcl$RMCL$RMTH-lag$MILAG-$MXLAG-$SIGTOMATCH-$SIGTOTRY-$FIRSTCUTOFF-$LASTCUTOFF-$CUTOFFINTERVAL.R &
					    
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


