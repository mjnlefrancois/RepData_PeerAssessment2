mapevent <- function(ev = character) {
    ev[ev %in% c("ASTRONOMICAL LOW TIDE")] <- "ASTRONOMICAL LOW TIDE"
    ev[ev %in% c("AVALANCE", "AVALANCHE")] <- "AVALANCHE"
    ev[ev %in% c("BLIZZARD", "BLIZZARD/WINTER STORM", "GROUND BLIZZARD")] <- "BLIZZARD"
    ev[ev %in% c("Beach Erosion", "COASTAL  FLOODING/EROSION", "COASTAL EROSION", 
                 "COASTAL FLOOD", "Coastal Flood", "COASTAL FLOODING", 
                 "Coastal Flooding", "COASTAL FLOODING/EROSION", "COASTAL STORM", 
                 "Coastal Storm", "COASTAL SURGE", "COASTALSTORM", 
                 "Erosion/Cstl Flood")] <- "COASTAL FLOOD"
    ev[ev %in% c("COLD", "Cold", "COLD AIR TORNADO", "COLD AND SNOW", 
                 "COLD AND WET CONDITIONS", "Cold Temperature", "COLD WAVE", 
                 "COLD WEATHER", "COLD/WIND CHILL", "COLD/WINDS", "COOL AND WET", 
                 "HYPOTHERMIA", "Hypothermia/Exposure", "HYPOTHERMIA/EXPOSURE", 
                 "LOW TEMPERATURE")] <- "COLD/WIND CHILL"
    ev[ev %in% c("ICE FLOES", "LANDSLIDE", "LANDSLIDES", "Landslump", "MUD SLIDE", "MUD SLIDES", "MUD SLIDES URBAN FLOODING", "MUDSLIDE", "Mudslide", "MUDSLIDES", "Mudslides", "ROCK SLIDE")] <- "DEBRIS FLOW"
    ev[ev %in% c("DENSE FOG", "FOG", "FOG AND COLD TEMPERATURES")] <- "DENSE FOG"
    ev[ev %in% c("DENSE SMOKE")] <- "DENSE SMOKE"
    ev[ev %in% c("DROUGHT", "DROUGHT/EXCESSIVE HEAT")] <- "DROUGHT"
    ev[ev %in% c("DUST DEVIL", "Dust Devil", "DUST DEVIL WATERSPOUT")] <- "DUST DEVIL"
    ev[ev %in% c("BLOWING DUST", "DUST STORM", "DUST STORM/HIGH WINDS")] <- "DUST STORM"
    ev[ev %in% c("EXCESSIVE HEAT", "EXTREME HEAT", "HEAT", "HEAT WAVE", 
                 "Heat Wave", "HEAT WAVE DROUGHT", "HEAT WAVES", 
                 "HYPERTHERMIA/EXPOSURE", "RECORD ", "HEAT", 
                 "RECORD/EXCESSIVE HEAT", "UNSEASONABLY WARM", 
                 "UNSEASONABLY WARM AND DRY", "WARM WEATHER")] <- "EXCESSIVE HEAT"
    ev[ev %in% c("Extended Cold", "EXTREME COLD", "Extreme Cold", 
                 "EXTREME COLD/WIND CHILL", "EXTREME WIND CHILL", 
                 "EXTREME WINDCHILL", "RECORD COLD", "Unseasonable Cold", 
                 "UNSEASONABLY COLD")] <- "EXTREME COLD/WIND CHILL"
    ev[ev %in% c(" FLASH FLOOD", "BREAKUP FLOODING", "DAM BREAK", "DROWNING", 
                 "FLASH FLOOD", "FLASH FLOOD - HEAVY RAIN", 
                 "FLASH FLOOD FROM ICE JAMS", "FLASH FLOOD LANDSLIDES", 
                 "FLASH FLOOD WINDS", "FLASH FLOOD/", "FLASH FLOOD/ STREET", 
                 "FLASH FLOOD/FLOOD", "FLASH FLOOD/LANDSLIDE", "FLASH FLOODING", 
                 "FLASH FLOODING/FLOOD", "FLASH FLOODING/THUNDERSTORM WI", 
                 "FLASH FLOODS", "FLOOD FLASH", "FLOOD/FLASH", 
                 "FLOOD/FLASH FLOOD", "FLOOD/FLASH/FLOOD", "FLOOD/FLASHFLOOD", 
                 "ICE JAM", "Ice jam flood (minor", "ICE JAM FLOODING")] <- "FLASH FLOOD"
    ev[ev %in% c("FLOOD", "FLOOD & HEAVY RAIN", "FLOOD/RAIN/WINDS", 
                 "FLOOD/RIVER FLOOD", "FLOODING", "FLOODING/HEAVY RAIN", 
                 "FLOODS", "MAJOR FLOOD", "MINOR FLOODING", 
                 "RIVER AND STREAM FLOOD", "RIVER FLOOD", "RIVER FLOODING", 
                 "River Flooding", "RURAL FLOOD", "SMALL STREAM FLOOD", 
                 "SNOWMELT FLOODING", "TIDAL FLOODING", "Tidal Flooding", 
                 "URBAN AND SMALL", "URBAN AND SMALL STREAM FLOODIN", 
                 "URBAN FLOOD", "URBAN FLOODING", "URBAN FLOODS", "URBAN SMALL", 
                 "URBAN/SMALL STREAM", "URBAN/SMALL STREAM FLOOD", 
                 "URBAN/SML STREAM FLD")] <- "FLOOD"
    ev[ev %in% c("FREEZING FOG", "GLAZE", "Glaze", "GLAZE ICE", 
                 "GLAZE/ICE STORM")] <- "FREEZING FOG"
    ev[ev %in% c("AGRICULTURAL FREEZE", "DAMAGING FREEZE", "Damaging Freeze", 
                 "Early Frost", "FREEZE", "Freeze", "FROST", "Frost/Freeze", 
                 "FROST/FREEZE", "FROST\\FREEZE", "ICE", "ICE AND SNOW", 
                 "ICE ON ROAD", "ICE ROADS", "ICE/STRONG WINDS", "ICY ROADS")] <- "FROST/FREEZE"
    ev[ev %in% c("FUNNEL CLOUD")] <- "FUNNEL CLOUD"
    ev[ev %in% c("HAIL", "HAIL 0.75", "HAIL 075", "HAIL 100", "HAIL 125", 
                 "HAIL 150", "HAIL 175", "HAIL 200", "HAIL 275", "HAIL 450", 
                 "HAIL 75", "HAIL DAMAGE", "HAIL/WIND", "HAIL/WINDS", "HAILSTORM", 
                 "SMALL HAIL")] <- "HAIL"
    ev[ev %in% c("EXCESSIVE RAINFALL", "EXCESSIVE WETNESS", "HEAVY RAIN", 
                 "HEAVY RAIN AND FLOOD", "Heavy Rain/High Surf", 
                 "HEAVY RAIN/LIGHTNING", "HEAVY RAIN/SEVERE WEATHER", 
                 "HEAVY RAIN/SMALL STREAM URBAN", "HEAVY RAIN/SNOW", 
                 "HEAVY RAINS", "HEAVY RAINS/FLOODING", "HEAVY SHOWER", 
                 "HVY RAIN", "RAIN", "RAIN/WIND", "RAINSTORM", 
                 "RECORD RAINFALL", "Torrential Rainfall", 
                 "UNSEASONAL RAIN")] <- "HEAVY RAIN"
    ev[ev %in% c("EXCESSIVE SNOW", "HEAVY LAKE SNOW", "HEAVY SNOW", 
                 "HEAVY SNOW AND HIGH WINDS", "HEAVY SNOW AND STRONG WINDS", 
                 "Heavy snow shower", "HEAVY SNOW SQUALLS", 
                 "HEAVY SNOW/BLIZZARD", "HEAVY SNOW/BLIZZARD/AVALANCHE", 
                 "HEAVY SNOW/FREEZING RAIN", "HEAVY SNOW/HIGH WINDS & FLOOD", 
                 "HEAVY SNOW/ICE", "HEAVY SNOW/SQUALLS", "HEAVY SNOW/WIND", 
                 "HEAVY SNOW/WINTER STORM", "HEAVY SNOWPACK", 
                 "HEAVY SNOW-SQUALLS", "RECORD SNOW", "SNOW ACCUMULATION", 
                 "SNOW AND HEAVY SNOW", "SNOW/HEAVY SNOW")] <- "HEAVY SNOW"
    ev[ev %in% c("   HIGH SURF ADVISORY", "HAZARDOUS SURF", "HEAVY SURF", 
                 "Heavy Surf", "Heavy surf and wind", 
                 "HEAVY SURF COASTAL FLOODING", "HEAVY SURF/HIGH SURF", 
                 "HEAVY SWELLS", "HIGH SEAS", "HIGH SURF", "High Surf", 
                 "HIGH SWELLS", "HIGH TIDES", "HIGH WATER", "HIGH WAVES", 
                 "ROUGH SURF")] <- "HIGH SURF"
    ev[ev %in% c("HIGH  WINDS", "HIGH WIND", "HIGH WIND (G40)", "HIGH WIND 48", 
                 "HIGH WIND AND SEAS", "HIGH WIND DAMAGE", "HIGH WIND/BLIZZARD", 
                 "HIGH WIND/HEAVY SNOW", "HIGH WIND/SEAS", "HIGH WINDS",
                 "HIGH WINDS HEAVY RAINS", "HIGH WINDS/", 
                 "HIGH WINDS/COASTAL FLOOD", "HIGH WINDS/COLD", 
                 "HIGH WINDS/HEAVY RAIN", "HIGH WINDS/SNOW", "WIND", "Wind", 
                 "WIND AND WAVE", "WIND DAMAGE", "Wind Damage", "WIND STORM", 
                 "WIND/HAIL", "WINDS")] <- "HIGH WIND"
    ev[ev %in% c("HURRICANE", "Hurricane Edouard", "HURRICANE EMILY", 
                 "HURRICANE ERIN", "HURRICANE FELIX", "HURRICANE GORDON", 
                 "HURRICANE OPAL", "HURRICANE OPAL/HIGH WINDS", 
                 "HURRICANE/TYPHOON", "HURRICANE-GENERATED SWELLS", 
                 "TYPHOON")] <- "HURRICANE/TYPHOON"
    ev[ev %in% c("ICE STORM", "ICE STORM/FLASH FLOOD")] <- "ICE STORM"
    ev[ev %in% c("Lake Effect Snow", "LAKE EFFECT SNOW", 
                 "LAKE-EFFECT SNOW")] <- "LAKE-EFFECT SNOW"
    ev[ev %in% c("LAKE FLOOD", "LAKESHORE FLOOD")] <- "LAKESHORE FLOOD"
    ev[ev %in% c("LIGHTING", "LIGHTNING", "LIGHTNING  WAUSEON", 
                 "LIGHTNING AND HEAVY RAIN", "LIGHTNING AND THUNDERSTORM WIN", 
                 "LIGHTNING FIRE", "LIGHTNING INJURY", 
                 "LIGHTNING THUNDERSTORM WINDS", "LIGHTNING.", 
                 "LIGHTNING/HEAVY RAIN", "LIGNTNING")] <- "LIGHTNING"
    ev[ev %in% c("MARINE HAIL")] <- "MARINE HAIL"
    ev[ev %in% c("MARINE HIGH WIND", "ROUGH SEAS")] <- "MARINE HIGH WIND"
    ev[ev %in% c("MARINE STRONG WIND")] <- "MARINE STRONG WIND"
    ev[ev %in% c("MARINE THUNDERSTORM WIND", 
                 "MARINE TSTM WIND")] <- "MARINE THUNDERSTORM WIND"
    ev[ev %in% c("?", "APACHE COUNTY", "HIGH", "Marine Accident", "MARINE MISHAP", "Other", "OTHER")] <- "UNKNOWN"
    ev[ev %in% c("RIP CURRENT", "RIP CURRENTS", 
                 "RIP CURRENTS/HEAVY SURF")] <- "RIP CURRENT"
    ev[ev %in% c("SEICHE")] <- "SEICHE"
    ev[ev %in% c("SLEET", "SLEET/ICE STORM")] <- "SLEET"
    ev[ev %in% c("ASTRONOMICAL HIGH TIDE", "HEAVY SEAS", "RAPIDLY RISING WATER", 
                 "ROGUE WAVE", "STORM SURGE", "STORM SURGE/TIDE")] <- "STORM SURGE/TIDE"
    ev[ev %in% c("GUSTY WIND", "GUSTY WIND/HAIL", "GUSTY WIND/HVY RAIN", 
                 "Gusty wind/rain", "GUSTY WINDS", "Gusty Winds", "Gusty winds", 
                 "NON TSTM WIND", "NON-SEVERE WIND DAMAGE", "NON-TSTM WIND", 
                 "STORM FORCE WINDS", "STRONG WIND", "Strong Wind", 
                 "STRONG WINDS", "Strong Winds")] <- "STRONG WIND"
    ev[ev %in% c(" TSTM WIND", " TSTM WIND (G45)", "DOWNBURST", "DRY MICROBURST", 
                 "DRY MIRCOBURST WINDS", "GUSTNADO", "MICROBURST", "Microburst", 
                 "MICROBURST WINDS", "SEVERE THUNDERSTORM", 
                 "SEVERE THUNDERSTORM WINDS", "SEVERE THUNDERSTORMS", 
                 "SEVERE TURBULENCE", "THUDERSTORM WINDS", "THUNDEERSTORM WINDS", 
                 "THUNDERESTORM WINDS", "THUNDERSTORM", "THUNDERSTORM  WINDS", 
                 "THUNDERSTORM DAMAGE TO", "THUNDERSTORM HAIL", 
                 "THUNDERSTORM WIND", "THUNDERSTORM WIND (G40)", 
                 "THUNDERSTORM WIND 60 MPH", "THUNDERSTORM WIND 65 MPH", 
                 "THUNDERSTORM WIND 65MPH", "THUNDERSTORM WIND 98 MPH", 
                 "THUNDERSTORM WIND G50", "THUNDERSTORM WIND G52", 
                 "THUNDERSTORM WIND G55", "THUNDERSTORM WIND G60", 
                 "THUNDERSTORM WIND TREES", "THUNDERSTORM WIND.", 
                 "THUNDERSTORM WIND/ TREE", "THUNDERSTORM WIND/ TREES", 
                 "THUNDERSTORM WIND/AWNING", "THUNDERSTORM WIND/HAIL", 
                 "THUNDERSTORM WIND/LIGHTNING", "THUNDERSTORM WINDS", 
                 "THUNDERSTORM WINDS 13", "THUNDERSTORM WINDS 63 MPH", 
                 "THUNDERSTORM WINDS AND", "THUNDERSTORM WINDS G60", 
                 "THUNDERSTORM WINDS HAIL", "THUNDERSTORM WINDS LIGHTNING", 
                 "THUNDERSTORM WINDS.", "THUNDERSTORM WINDS/ FLOOD", 
                 "THUNDERSTORM WINDS/FLOODING", "THUNDERSTORM WINDS/FUNNEL CLOU", 
                 "THUNDERSTORM WINDS/HAIL", "THUNDERSTORM WINDS53", 
                 "THUNDERSTORM WINDSHAIL", "THUNDERSTORM WINDSS", 
                 "THUNDERSTORM WINS", "THUNDERSTORMS", "THUNDERSTORMS WIND", 
                 "THUNDERSTORMS WINDS", "THUNDERSTORMW", "THUNDERSTORMWINDS", 
                 "THUNDERSTROM WIND", "THUNDERTORM WINDS", "THUNERSTORM WINDS", 
                 "TSTM WIND", "Tstm Wind", "TSTM WIND  (G45)", "TSTM WIND (41)", 
                 "TSTM WIND (G35)", "TSTM WIND (G40)", "TSTM WIND (G45)", 
                 "TSTM WIND 40", "TSTM WIND 45", "TSTM WIND 55", "TSTM WIND 65)", 
                 "TSTM WIND AND LIGHTNING", "TSTM WIND DAMAGE", "TSTM WIND G45", 
                 "TSTM WIND G58", "TSTM WIND/HAIL", "TSTM WINDS", "TSTMW", 
                 "TUNDERSTORM WIND", "WET MICROBURST", "Whirlwind", 
                 "WHIRLWIND")] <- "THUNDERSTORM WIND"
    ev[ev %in% c("LANDSPOUT", "TORNADO", "TORNADO F0", "TORNADO F1", "TORNADO F2", 
                 "TORNADO F3", "TORNADOES", "TORNADOES, TSTM WIND, HAIL", 
                 "TORNDAO", "WATERSPOUT", "WATERSPOUT-", "WATERSPOUT TORNADO", 
                 "WATERSPOUT/ TORNADO", "WATERSPOUT/TORNADO", 
                 "WATERSPOUT-TORNADO")] <- "TORNADO"
    ev[ev %in% c("GRADIENT WIND", "gradient wind", "Gradient wind", 
                 "TROPICAL DEPRESSION")] <- "TROPICAL DEPRESSION"
    ev[ev %in% c("TROPICAL STORM", "TROPICAL STORM ALBERTO", 
                 "TROPICAL STORM DEAN", "TROPICAL STORM GORDON", 
                 "TROPICAL STORM JERRY")] <- "TROPICAL STORM"
    ev[ev %in% c("TSUNAMI")] <- "TSUNAMI"
    ev[ev %in% c("VOLCANIC ASH")] <- "VOLCANIC ASH"
    ev[ev %in% c("BRUSH FIRE", "FOREST FIRES", "GRASS FIRES", 
                 "WILD FIRES", "WILD/FOREST FIRE", "WILD/FOREST FIRES", 
                 "WILDFIRE", "WILDFIRES")] <- "WILDFIRE"
    ev[ev %in% c("WINTER STORM", "WINTER STORM HIGH WINDS", 
                 "WINTER STORMS")] <- "WINTER STORM"
    ev[ev %in% c("BLACK ICE", "BLOWING SNOW", "blowing snow", 
                 "FALLING SNOW/ICE", "FREEZING DRIZZLE", "Freezing Drizzle", 
                 "Freezing drizzle", "FREEZING RAIN", "Freezing Rain", 
                 "FREEZING RAIN/SLEET", "FREEZING RAIN/SNOW", "Freezing Spray", 
                 "HARD FREEZE", "HEAVY MIX", "HEAVY PRECIPITATION", 
                 "LATE SEASON SNOW", "LIGHT FREEZING RAIN", "Light snow", 
                 "Light Snow", "LIGHT SNOW", "Light Snowfall", "MIXED PRECIP", 
                 "Mixed Precipitation", "MIXED PRECIPITATION", "RAIN/SNOW", 
                 "SNOW", "Snow", "SNOW AND ICE", "SNOW AND ICE STORM", 
                 "SNOW FREEZING RAIN", "SNOW SQUALL", "SNOW SQUALLS", 
                 "Snow Squalls", "SNOW/ BITTER COLD", "SNOW/ ICE", 
                 "SNOW/BLOWING SNOW", "SNOW/COLD", "SNOW/FREEZING RAIN", 
                 "SNOW/HIGH WINDS", "SNOW/ICE", "SNOW/ICE STORM", "SNOW/SLEET", 
                 "SNOW/SLEET/FREEZING RAIN", "THUNDERSNOW", "WINTER WEATHER", 
                 "WINTER WEATHER MIX", "WINTER WEATHER/MIX", "Wintry Mix", 
                 "WINTRY MIX")] <- "WINTER WEATHER"
    return(ev)
}