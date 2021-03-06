# Nestable-coordinate-system-for-Tikz-circuits
More spaces can be easily allocated at any point in the new coordinate system for other circuits to be inserted in and connected to without changing the logic of the current circuit.

         Gang Liu (gl.cell@outlook, http://orcid.org/0000-0003-1575-9290)
              and
         Shiwei Huang (huang937@gmail.com)

The CircuiTikz package is very helpful in circuit design. Here a new coordinate system can be used together with it. Unlimited specific instances of the coordinate system can be generated by running the FORTRAN code coordinates.f90. Each is identified with a keyword of lower-case letters, but denoted as KEYWORD here, then a file called coordKEYWORD.tex is generated for use as the coordinate instance. The coordKEYWORD.tex file sets the following macros:

    \pgfmathsetmacro{\KEYWORDxxxspacing}{1}
    \pgfmathsetmacro{\KEYWORDyyyspacing}{1}
    \pgfmathsetmacro{\KEYWORDxxxa}{-8}
    \pgfmathsetmacro{\KEYWORDyyya}{-8}

    \pgfmathsetmacro{\KEYWORDxxxb}{\KEYWORDxxxa + \KEYWORDxxxspacing + 0.0 }
    \pgfmathsetmacro{\KEYWORDxxxc}{\KEYWORDxxxb + \KEYWORDxxxspacing + 0.0 }
    \pgfmathsetmacro{\KEYWORDxxxd}{\KEYWORDxxxc + \KEYWORDxxxspacing + 0.0 }
        ...
    \pgfmathsetmacro{\KEYWORDxxxz}{\KEYWORDxxxy + \KEYWORDxxxspacing + 0.0 }

    \pgfmathsetmacro{\KEYWORDyyyb}{\KEYWORDyyya + \KEYWORDyyyspacing + 0.0 }
    \pgfmathsetmacro{\KEYWORDyyyc}{\KEYWORDyyyb + \KEYWORDyyyspacing + 0.0 }
        ...
    \pgfmathsetmacro{\KEYWORDyyyz}{\KEYWORDyyyy + \KEYWORDyyyspacing + 0.0 }

    \coordinate (KEYWORDpppaa) at (\KEYWORDxxxa, \KEYWORDyyya);
    \coordinate (KEYWORDpppab) at (\KEYWORDxxxa, \KEYWORDyyyb);
    \coordinate (KEYWORDpppac) at (\KEYWORDxxxa, \KEYWORDyyyc);
        ...
    \coordinate (KEYWORDpppzy) at (\KEYWORDxxxz, \KEYWORDyyyy);
    \coordinate (KEYWORDpppzz) at (\KEYWORDxxxz, \KEYWORDyyyz);
    
where the coordinate setting is for all combinations of the xxx and yyy components. Any of the xxx and yyy components can be manually changed later to allocate spaces from the coordinate, then coordinates after it will be pushed the same distance as a whole.  Another way to get a new instance of the coordinate system is to copy the file coorddemobygangliu.tex in the example01 into another then substitute all "demobygangliu" strings with the chosen KEYWORD inside the new file.

If circuits and/or parts of them are created by different specific instances of the coordinate system even by many people, they can be easily integrated, espectially some of them can be inserted into others then connected. 

The new commands

    \coordinatebackground
    \coordinatebackgroundxy 
    
for showing the coordinate as reference backgrounds are coded in the file 
    afewmorecommandsintikzpiture.tex
where their usages are also given. They can only be used in the Tikz picture environment. 


More derails are shown in the example(s). 


