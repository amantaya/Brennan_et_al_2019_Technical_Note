
#change the name to the fix file to process
Fix_file="Fix_example.txt"

#change the name to the sensor file to process
Sensor_file="Sensor_example.txt"

#change the name to the animal id
Animal_ID="Steer_123"

#change graze to no if you do not want that output
Graze="Yes"



load("Lotek_Function.Rdata")
Lotek_Function(Fix_file=Fix_file,Sensor_file=Sensor_file,Graze=Graze)



