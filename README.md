# oversightboard_comments

R Code for analysis of public comments on the Oversight Board decision on Donals Trump's Facebook Ban. 

More information:
https://osbcontent.s3-eu-west-1.amazonaws.com/PC+Appendix+2021-001-FB-FBR.pdf.

What the code does (UPDATE: 2021-05-20): 
1. extract the long and short form of the comments from the pdf,
2. decide which is more informative (contains more words) - often the long form just says "see above" - then choose the relevant comment.
3. pre-processing...
4. topic modeling of the comments.
