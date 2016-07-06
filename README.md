# UIASReader

This program is for reading the UIAS (Unique Identifier Audit System) files provided by the New York State Education Department via the IRS Portal (currently) or L2RPT (coming soon).
It currently handles the Simultaneous Enrollments, Disappearing Students, False Transfers, False Transfers Across Years, and False Dropouts files.
In the IRS Portal, the files are provided in .xls format, but must be saved as .xlsx format before UIASReader can use them.
In L2RPT, files downloaded as "Excel 2007 Data" can be used by UIASReader.

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

1. Comparison to prior month
1. Graphic user interface using Shiny
1. Incorporate the Demographics (Student Lite) extract and the Enrollment extract from the student management system
1. Support for the new file formats delivered via L2RPT.
