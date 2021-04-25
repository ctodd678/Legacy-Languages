       identification division.
       program-id. statsnew.

       environment division.
       input-output section.
       file-control.
       select input-file assign to in-file
       organization is line sequential.
       select output-file assign to out-file
       organization is line sequential.



       data division.
       file section.
       fd input-file.
       01 sample-input pic x(80).
       fd output-file.
       01 output-line pic x(80).

       working-storage section.
       77 sx picture s9(14)v9(4) usage is computational-3.
       77 sdev picture s9(20)v9(8) usage is computational-3.
       77 n picture s9999 usage is computational.
       77 m picture s9(20)v9(8) usage is computational-3.
       77 m1 picture s9(30)v9(8) usage is computational-3.
       77 m2 picture s9(30)v9(8) usage is computational-3.
       77 root picture s9(14)v9(4) usage is computational-3.
       77 i picture s9999 usage is computational.
       77 std picture s9(25)v9(12) usage is computational-3.
       77 temp picture s9(24)v9(12) usage is computational-3.
       01 array-area.
           02 x picture s9(14)v9(4) usage is computational-3
           occurs 1000 times.
       01 input-value.
           02 in-x picture s9(14)v9(4).
           02 filler picture x(62).
       01 in-file.
           02 filler picture x(255).
       01 out-file.
           02 filler picture x(255).
       01 title-line.
           02 filler picture x(50) value
           ' CIS3110 A3 - Cobol Data Statistics'.
       01 under-line.
           02 filler picture x(45)
           value '-----------------------------------------'.
       01 col-heads.
           02 filler picture x(21) value ' Input Values'.
       01 data-line.
           02 filler picture x(5) value spaces.
           02 out-x picture -(14)9.9(4).
       01 print-line-1.
           02 filler picture x(20) value ' Mean = '.
           02 out-mn picture -(14)9.9(4).
       01 print-line-2.
           02 filler picture x(24) value ' Standard Deviation = '.
           02 out-st picture -(10)9.9(4).
       01 print-line-3.
           02 filler picture x(20) value ' Quadratic Mean = '.
           02 out-gm picture -(14)9.9(4).
       01 print-line-4.
           02 filler picture x(20) value ' Harominc Mean = '.
           02 out-hm picture -(14)9.9(4).
       01 print-line-5.
           02 filler picture x(20) value ' Median = '.
           02 out-med picture -(14)9.9(4).
       01 print-line-6.
           02 filler picture x(20) value ' Variance = '.
           02 out-var picture -(14)9.9(4).
       
       procedure division.
       display 'Please enter input file name:'.
       accept in-file from sysin.
       display 'Please enter output file name:'.
       accept out-file from sysin.
       open input input-file, output output-file.

       write output-line from title-line after advancing 0 lines.
       write output-line from under-line after advancing 1 lines.
       write output-line from col-heads after advancing 1 lines.
       write output-line from under-line after advancing 1 lines.
       move zero to sx.

       perform input-loop varying n from 1 by 1
           until n is greater than 1000.

       input-loop.
           read input-file into input-value at end perform b1.
           move in-x to x(n), out-x.
           write output-line from data-line after advancing 1 line.
           compute sx = sx + x(n).

       b1.
           compute n = n - 1.
           compute m = sx / n.
           perform sum-loop varying i from 1 by 1 until i is greater than n.
           compute std = (sdev / (n - 1)) ** 0.5.
           write output-line from under-line after advancing 1 line.
           move m to out-mn.
           move std to out-st.
           perform calc-geo-mean.
           move m1 to out-gm.
           perform calc-har-mean.
           move m1 to out-hm.
           perform calc-median.
           move m1 to out-med.
           perform calc-variance.
           move m1 to out-var.
           write output-line from print-line-1 after advancing 1 line.
           write output-line from print-line-2 after advancing 1 line.
           write output-line from print-line-3 after advancing 1 line.
           write output-line from print-line-4 after advancing 1 line.
           write output-line from print-line-5 after advancing 1 line.
           write output-line from print-line-6 after advancing 1 line.
           perform finish.

       sum-loop.
           compute temp = x(i) - m.
           compute temp = temp * temp.
           compute sdev = sdev + temp.

       geo-loop.
           compute m1 = m1 + x(i) * x(i).
       
       har-loop.
           compute temp = 1 / x(i).
           compute m1 = m1 + temp.

       calc-geo-mean.
           set m1 to 0.
           set root to 1.
           compute root = root / n.
           perform geo-loop varying i from 1 by 1 until i is greater than n.
           compute m1 = m1 / n.
           compute m1 = m1 ** 0.5.

       calc-har-mean.
           set m1 to 0.
           perform har-loop varying i from 1 by 1 until i is greater than n.
           compute m1 = n / m1.

       calc-median.
           set m1 to 0.
           sort x descending.
           compute i rounded = (n) / 2.
           compute m1 = x(i) + x(i + 1).
           compute m1 = m1 / 2.

       calc-variance.
           compute m1 = std * std.

       finish.
           close input-file, output-file.
           stop run.
