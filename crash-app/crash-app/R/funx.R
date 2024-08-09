col2 = function(data, x) {
  if (x == "Fatalities") {
    return (data$Fatalities)
  } else if (x == "Speeding") {
    return (data$Speeding)
  } else if (x == "Alcohol") {
    return (data$Alcohol)
  } else if (x == "Distracted") {
    return (data$Distracted)
  } else if (x == "Prior") {
    return (data$Prior)
  }
}

ylabel = function(x) {
  if (x == "Fatalities") {
    return ("Number of Fatalities per Billion Miles Driven")
  } else if (x == "Speeding") {
    return ("Percentage of Drivers Speeding While in Fatal Collision")
  } else if (x == "Alcohol") {
    return ("Percentage of Drivers Impaired by Alcohol While in Fatal Collision")
  } else if (x == "Distracted") {
    return ("Percentage of Drivers Distracted While in Fatal Collision")
  } else if (x == "Prior") {
    return ("Percentage of Drivers in Fatal Collision with Prior Accident")
  }
}

comp_label = function(x) {
  if (x == "Fatalities") {
    return ("NA")
  } else if (x == "Speeding") {
    return ("Number of Fatalities Speeding vs. Total Number of Fatalities")
  } else if (x == "Alcohol") {
    return ("Number of Fatalities Impaired by Alcohol vs. Total Number of Fatalities")
  } else if (x == "Distracted") {
    return ("Number of Fatalities Distracted vs. Total Number of Fatalities")
  } else if (x == "Prior") {
    return ("Number of Fatalities with Prior Accident vs. Total Number of Fatalities")
  }
}