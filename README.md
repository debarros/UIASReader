# UIASReader

This program is for reading the UIAS ((Unique Identifier Audit System) files provided by the New York State Education Department via the IRS Portal.
It currently handles the Simultaneous Enrollments, False Transfers, and False Transfers Across Years files.
The files are provided by NYSED in .xls format, but must be saved as .xlsx format before UIAS reader can use them.

##The neat part

There are way too many types of errors included in the False Transfers report.  
This program separates out the actual false transfers from the overlapping enrollments.
It then categorizes overlapping enrollments into the following types:
1. We both tried to claim the student at the beginning of the year
1. exact same enrollment
1. Same exit date
1. Same entry date
1. We entered too soon or they exited too late
1. We exited too late or they entered too soon
1. Our entire enrollment is during theirs
1. Their entire enrollment is during ours

##Future planned functionality:

1. False Dropout files
1. Comparison to prior month
1. Graphic user interface using Shiny
