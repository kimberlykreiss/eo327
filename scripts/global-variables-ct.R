library(tidyverse)
library(openxlsx)
library(bgi)
library(lubridate)
library(scales)

## need to figure out how to more accurately identify government jobs in NJ
govt_firms <- c(
  "State Of Connecticut",
  "City Of",
  "County Of",
  "District",
  "Authority",
  "Commission",
  "Connecticut Courts",
  "Connecticut Economic Development Authority",
  "Connecticut Housing Mortgage Finance Agency",
  "Connecticut Department of Health",
  "Library",
  "Connecticut Division",
  "Department",
  "Township",
  "Town Of",
  "County Board",
  "Borough Of"
)
school_related <-
  c(
    "District",
    "Charter School",
    "Community College",
    "Board of Education",
    "Regional High School",
    "Elementary School",
    "Middle School",
    "Public School",
    "School District",
    "University of Connecticut",
    "Central Connecticut State University",
    "Eastern Connecticut State University",
    "Southern Connecticut State University",
    "Western Connecticut State University",
    "Charter Oak State College",
    "Connecticut State Library",
    "Norwalk Community College",
    "Housatonic Community College",
    "Gateway Community College",
    "Manchester Community College",
    "Middlesex Community College",
    "Naugatuck Valley Community College",
    "Northwestern Connecticut Community College",
    "Quinebaug Valley Community College",
    "Three Rivers Community College",
    "Tunxis Community College"
  )

CT_counties <- c("Fairfield County", "Hartford County", "Litchfield County", "Middlesex County", "New Haven County", "New London County", "Tolland County", "Windham County")


CT_towns <- c("Andover", "Ansonia", "Ashford", "Avon", "Barkhamsted", "Beacon Falls", "Berlin", "Bethany", 
              "Bethel", "Bethlehem", "Bloomfield", "Bolton", "Bozrah", "Branford", "Bridgeport", "Bridgewater", 
              "Bristol", "Brookfield", "Brooklyn", "Burlington", "Canaan", "Canterbury", "Canton", "Chaplin", 
              "Cheshire", "Chester", "Clinton", "Colchester", "Colebrook", "Columbia", "Cornwall", "Coventry", 
              "Cromwell", "Danbury", "Darien", "Deep River", "Derby", "Durham", "East Granby", "East Haddam", 
              "East Hampton", "East Hartford", "East Haven", "East Lyme", "East Windsor", "Eastford", "Easton", 
              "Ellington", "Enfield", "Essex", "Fairfield", "Farmington", "Franklin", "Glastonbury", "Goshen", 
              "Granby", "Greenwich", "Griswold", "Groton", "Guilford", "Haddam", "Hamden", "Hampton", "Hartford", 
              "Hartland", "Harwinton", "Hebron", "Kent", "Killingly", "Killingworth", "Lebanon", "Ledyard", 
              "Lisbon", "Litchfield", "Lyme", "Madison", "Manchester", "Mansfield", "Marlborough", "Meriden", 
              "Middlebury", "Middlefield", "Middletown", "Milford", "Monroe", "Montville", "Morris", "Naugatuck", 
              "New Britain", "New Canaan", "New Fairfield", "New Hartford", "New Haven", "New London", "New Milford", 
              "Newington", "Newtown", "Norfolk", "North Branford", "North Canaan", "North Haven", "North Stonington", 
              "Norwalk", "Norwich", "Old Lyme", "Old Saybrook", "Orange", "Oxford", "Plainfield", "Plainville", 
              "Plymouth", "Pomfret", "Portland", "Preston", "Prospect", "Putnam", "Redding", "Ridgefield", 
              "Rocky Hill", "Roxbury", "Salem", "Salisbury", "Scotland", "Seymour", "Sharon", "Shelton", 
              "Sherman", "Simsbury", "Somers", "South Windsor", "Southbury", "Southington", "Sprague", 
              "Stafford", "Stamford", "Sterling", "Stonington", "Stratford", "Suffield", "Thomaston", 
              "Thompson", "Tolland", "Torrington", "Trumbull", "Union", "Vernon", "Voluntown", "Wallingford", 
              "Warren", "Washington", "Waterbury", "Waterford", "Watertown", "West Hartford", "West Haven", 
              "Westbrook", "Weston", "Westport", "Wethersfield", "Willington", "Wilton", "Winchester", 
              "Windham", "Windsor", "Windsor Locks", "Wolcott", "Woodbridge", "Woodbury", "Woodstock")
