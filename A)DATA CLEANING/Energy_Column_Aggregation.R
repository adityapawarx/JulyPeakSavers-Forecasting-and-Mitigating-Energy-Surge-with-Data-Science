library(tidyverse)
path <- '/Users/subhiksha/Documents/IDS/ids/energydataaa_idsProj.csv'
data <- read_csv(path)

#view(data111)
head(data,100)
colnames(data)

# kitchen
data$out.kitchen.energy_consumption <- data$out.electricity.range_oven.energy_consumption +
  data$out.electricity.dishwasher.energy_consumption +
  data$out.electricity.refrigerator.energy_consumption +
  data$out.electricity.freezer.energy_consumption +
  data$out.natural_gas.range_oven.energy_consumption +
  data$out.natural_gas.grill.energy_consumption +
  data$out.propane.range_oven.energy_consumption

# laundry
data$out.laundry.energy_consumption <- data$out.electricity.clothes_dryer.energy_consumption +
  data$out.natural_gas.clothes_dryer.energy_consumption +
  data$out.electricity.clothes_washer.energy_consumption +
  data$out.propane.clothes_dryer.energy_consumption

# heating_cooling
data$out.heating_cooling.energy_consumption <- data$out.electricity.heating_fans_pumps.energy_consumption +
  data$out.electricity.heating_hp_bkup.energy_consumption +
  data$out.electricity.heating.energy_consumption +
  data$out.electricity.cooling.energy_consumption +
  data$out.natural_gas.heating_hp_bkup.energy_consumption +
  data$out.natural_gas.heating.energy_consumption +
  data$out.propane.heating_hp_bkup.energy_consumption +
  data$out.propane.heating.energy_consumption +
  data$out.fuel_oil.heating_hp_bkup.energy_consumption +
  data$out.fuel_oil.heating.energy_consumption +
  data$out.natural_gas.fireplace.energy_consumption +
  data$out.electricity.cooling_fans_pumps.energy_consumption

# water_heating
data$out.water_heating.energy_consumption <- data$out.electricity.hot_water.energy_consumption +
  data$out.fuel_oil.hot_water.energy_consumption +
  data$out.natural_gas.hot_water.energy_consumption +
  data$out.propane.hot_water.energy_consumption

# electrical_appliances
data$out.electrical_appliances.energy_consumption <- data$out.electricity.lighting_exterior.energy_consumption +
  data$out.electricity.lighting_garage.energy_consumption +
  data$out.electricity.lighting_interior.energy_consumption +
  data$out.electricity.plug_loads.energy_consumption +
  data$out.electricity.mech_vent.energy_consumption +
  data$out.natural_gas.lighting.energy_consumption +
  data$out.electricity.ceiling_fan.energy_consumption

# outdoor_appliances
data$out.outdoor_appliances.energy_consumption <- data$out.electricity.hot_tub_heater.energy_consumption +
  data$out.electricity.hot_tub_pump.energy_consumption +
  data$out.electricity.pool_heater.energy_consumption +
  data$out.electricity.pool_pump.energy_consumption +
  data$out.natural_gas.hot_tub_heater.energy_consumption +
  data$out.natural_gas.pool_heater.energy_consumption +
  data$out.electricity.well_pump.energy_consumption

# renewable_energy
data$out.renewable_energy.energy_consumption <- data$out.electricity.pv.energy_consumption

#total
data$out.total.energy_consumption <- data$out.electricity.range_oven.energy_consumption +
  data$out.electricity.dishwasher.energy_consumption +
  data$out.electricity.refrigerator.energy_consumption +
  data$out.electricity.freezer.energy_consumption +
  data$out.natural_gas.range_oven.energy_consumption +
  data$out.natural_gas.grill.energy_consumption +
  data$out.propane.range_oven.energy_consumption +
  data$out.electricity.clothes_dryer.energy_consumption +
  data$out.natural_gas.clothes_dryer.energy_consumption +
  data$out.electricity.clothes_washer.energy_consumption +
  data$out.propane.clothes_dryer.energy_consumption +
  data$out.electricity.heating_fans_pumps.energy_consumption +
  data$out.electricity.heating_hp_bkup.energy_consumption +
  data$out.electricity.heating.energy_consumption +
  data$out.electricity.cooling.energy_consumption +
  data$out.natural_gas.heating_hp_bkup.energy_consumption +
  data$out.natural_gas.heating.energy_consumption +
  data$out.propane.heating_hp_bkup.energy_consumption +
  data$out.propane.heating.energy_consumption +
  data$out.fuel_oil.heating_hp_bkup.energy_consumption +
  data$out.fuel_oil.heating.energy_consumption +
  data$out.natural_gas.fireplace.energy_consumption +
  data$out.electricity.cooling_fans_pumps.energy_consumption +
  data$out.electricity.hot_water.energy_consumption +
  data$out.fuel_oil.hot_water.energy_consumption +
  data$out.natural_gas.hot_water.energy_consumption +
  data$out.propane.hot_water.energy_consumption +
  data$out.electricity.lighting_exterior.energy_consumption +
  data$out.electricity.lighting_garage.energy_consumption +
  data$out.electricity.lighting_interior.energy_consumption +
  data$out.electricity.plug_loads.energy_consumption +
  data$out.electricity.mech_vent.energy_consumption +
  data$out.natural_gas.lighting.energy_consumption +
  data$out.electricity.ceiling_fan.energy_consumption +
  data$out.electricity.hot_tub_heater.energy_consumption +
  data$out.electricity.hot_tub_pump.energy_consumption +
  data$out.electricity.pool_heater.energy_consumption +
  data$out.electricity.pool_pump.energy_consumption +
  data$out.natural_gas.hot_tub_heater.energy_consumption +
  data$out.natural_gas.pool_heater.energy_consumption +
  data$out.electricity.well_pump.energy_consumption +
  data$out.electricity.pv.energy_consumption

colnames(data[,1:45])
colnames(data[,45:54])
data_dependent <- data[,45:54]
write.csv(data_dependent, file = "/Users/subhiksha/Documents/IDS/ids/Final_energy_data.csv", row.names = TRUE)

dim(data_dependent)

