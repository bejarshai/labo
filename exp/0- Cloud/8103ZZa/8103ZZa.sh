tabulador="	"
exp_name=8103ZZa
echo "experiment	timestamp	event"  >  log.txt 
fecha0=$(date +"%Y%m%d %H%M%S") 
echo "$exp_name""$tabulador""$fecha0""$tabulador""SH_START" >> log.txt 
Rscript --vanilla ~/labo/src/lib/exp_run_init.r  8103ZZa
Rscript --vanilla ~/labo/src/completo/z991_ZZ_lightgbm.r  8103ZZa  2>&1 | tee outfile 
cp  ~/log/*.txt  ./  
Rscript --vanilla ~/labo/src/lib/exp_run_end.r
fecha1=$(date +"%Y%m%d %H%M%S") 
echo "$exp_name""$tabulador""$fecha1""$tabulador""SH_END" >> log.txt 
find ./ ! -name "*.gz" ! -name . -exec cp -prt /media/expshared/bejar_shai/exp/8103ZZa/  {} +  

#suicidio
export NAME=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/name -H 'Metadata-Flavor: Google') 
export ZONE=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/zone -H 'Metadata-Flavor: Google') 
gcloud --quiet compute instances delete $NAME --zone=$ZONE 
