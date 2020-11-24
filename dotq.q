\d .q

/ Atomic
k)neg:-:                  / Negate
k)not:~:                  / Not zero
k)null:^:                 / Is null
k)string:$:               / Cast to string
k)reciprocal:%:           / Reciprocal
k)floor:_:                / Round down 
ceiling:{neg floor neg x} / Round up: negate x so that floor has the opposite effect, revert
signum:{(x>0)-x<0}        / Sign: null or neg, zero and positive return -1i, 0i and 1i, respectively 
/ - Infix
and:&                                       / Lesser
or:|                                        / Greater
mod:{x-y*x div y}                           / Modulus
xbar:{x*y div x:$[16h=abs type x;"j"$x;x]}  / Round down to the nearest multiple of x
xlog:{log[y]%log x}                         / Logarithm: change of base law: log x (y) = ln(y) / ln(x)

/ Aggregate
k)count:#:                                        / Number of items
k)first:*:                                        / First item
svar:{(n*var x)%(neg 1)+n:(count x)-sum null x}   / Sample variance: ignoring nulls, svar = ( n / n - 1 ) * var
sdev:{sqrt svar x}                                / Sample standard deviation
med:{avg x(iasc x)floor .5*(neg 1;0)+count x,:()} / Median: middlesmost indices applied to indices for sorting, applied to x and averaged
all:min"b"$                                       / Everything is true (non-zero)
any:max"b"$                                       / Anything is true (non-zero)
/ - Infix
scov:{(n*cov[x;y])%(neg 1)+n:(count x)-sum null x+y}  / Sample covariance: ignoring nulls, scov = ( n / n - 1 ) * cov(x,y)

/ Uniform
sums:+\                                           / Running totals
prds:*\                                           / Running products
mins:&\                                           / Running minimums
maxs:|\                                           / Running maximums
fills:^\                                          / Replace nulls with preceding non-nulls
deltas:-':                                        / Differences between adjacent items
ratios:%':                                        / Ratios between successive items
avgs:{(sums x)%sums not null x}                   / Running totals divided by running counts (ignoring nulls)
differ:{not(~)prior x}                            / Where list items change value (match now returns boolean making cast redundant)
prev: :':                                         / Previous items in a list
next:{$[0h>type x;'`rank;1_x,enlist x 0N]}        / Next items in a list (index using null to get null of correct type)
reverse:|:                                        / Reverse order
rank:{$[0h>type x;'`rank;<<x]}                    / Position in sorted list
iasc:{$[0h>type x;'`rank;<x]}                     / Indexes required to sort in ascending order
idesc:{$[0h>type x;'`rank;>x]}                    / Indexes required to sort in descending order
asc:{                                             / Ascending sort
  $[99h=type x;(key x)[i]!`s#r i:iasc r:value x;    / If dict, sort by value
    `s=attr x;x;                                      / If sorted, return as is 
    0h>type x;'`rank;                                 / If atom, throw error
    `s#x iasc x]}                                     / Else, sort
desc:{                                            / Descending sort
  $[99h=type x;(key x)[i]!r i:idesc r:value x;      / If dict, sort by value
    0h>type x;'`rank;                                 / If atom, throw error
    x idesc x]}                                       / Else, sort
/ - Infix
msum:{                                                / x-item moving sums of y
  $[99h=type y;(key y)!.z.s[x;value y];                 / If dict, apply self to value
    y-(neg x)_(0i*x#y),y:sums y]}                         / Else, x-item moving sums = sums - x-shifted sums 
mcount:{msum[x;not null y]}                           / x-item moving count of the non-null items of y
mavg:{msum[x;0.0^y]%mcount[x;y]}                      / x-item moving averages of y
mdev:{sqrt mavg[x;y*y]-m*m:mavg[x;y:"f"$y]}           / x-item moving deviations of y
xrank:{$[0h>type y;'`rank;floor y*x%count y:rank y]}  / Group by value: scale rank value by buckets per item 
mmin:{(x-1)prior[and;]/y}                             / x-item moving minimums of y
mmax:{(x-1)prior[or;]/y}                              / x-item moving maximums of y
xprev:{$[0h>type y;'`rank;y(til count y)-x]}          / Shift indices by x and apply to y
rotate:{                                              / Shift list items left or right
  $[0h>type y;'`rank;                                   / If atom, throw error
    98h<type y;'`type;                                    / If dict, function, iterator or derived funciton, throw error
    count y;raze reverse(0;x mod count y)cut y;           / If positive count, rotate
    y]}                                                   / Else i.e. empty list, return as is
ema:{(first y)(1f-x)\x*y}                             / Exponential moving average

/ TODO - below this line
/
/other: ~,#_ !.   getenv exit
rand:{first 1?x} / Numeric atom in the range [0,x)
distinct:?:;group:=:;where:&:;flip:+:;type:@:;key:!:;til:{$[0>@x;!x;'`type]};value:get:.:;attr:-2!;cut:{$[0h>@x;x*!-_-(#y)%x;x]_y}
set:{$[@x;.[x;();:;y];-19!((,y),x)]};upsert:.[;();,;] / :: ,: files?
raze:,/;union:?,;inter:{x@&x in y};except:{x@&~x in y};cross:{n:#m:&(#x)##y;$[99h=@x;((!x)[m],'n#!y)!(. x)[m],'n#. y;((),x)[m],'n#y]} /extant:{x@&~^x}
sv:{x/:y};vs:{x\:y};sublist:{$[99h=@y;sublist[x;!y]!sublist[x;. y];~0h>@x;$[.Q.qp y;.Q.ind[y];y]i+!"j"$0|x[1]&(#y)-i:*x;abs[x]<#y;x#y;y]}

// Apply monad to the items of its argument
each:{x'y}
// Covers for adverbs over and scan
// If x is monadic, apply until convergence
// If x is dyadic, fold by applying progressively between succesive items 
over:{x/y}
scan:{x\y}
// Apply dyad to each item and the item preceding it
prior:{x':y}

// Matrices
// Multiply, dot product
k)mmu:$
// Least squares, divide
k)lsq:!
// Inverse
k)inv:!:

// Hashes
// Message digest hash
md5:-15!

// Timezones
// Local equivalent of UTC timestamp
k)ltime:%:
// UTC equivalent of local timestamp
// Local time plus magnitude and direction of offset: x+x-ltime x
// The second part must cover an edge case that I can't think of right now
gtime:{t+x-ltime t:x+x-ltime x}

/file&comm
read0:0::;read1:1::;hclose:>:;hdel:~:;hsym:"s"$-1!';hcount:-7!;peach:{x':y};system:."\\",

/string:  like ss
ltrim:{$[~t&77h>t:@x;.z.s'x;^*x;((^x)?0b)_x;x]};rtrim:{$[~t&77h>t:@x;.z.s'x;^last x;(-(|^x)?0b)_x;x]};trim:{ltrim rtrim x}
lower:{$[$[(~@x)&10h~@*x;&/10h=@:'x;0b];_x;~t&77h>t:abs@@x;.z.s'x;19<t;.z.s@. x;~t in 10 11h;'`type;_x]}
upper:{$[$[(~@x)&10h~@*x;&/10h=@:'x;0b];.Q.Aa x;~t&77h>t:abs@@x;.z.s'x;19<t;.z.s@. x;~t in 10 11h;'`type;$[11=t;`$.Q.Aa@$x;.Q.Aa x]]}
ssr:{,/@[x;1+2*!_.5*#x:(0,/(0,{n:x?"[";$[n=#x;n;n+.z.s$[(#x)=p:x?"]";'"unmatched ]";p]_x:(n+2+"^"=x n+1)_x]}y,"")+/:x ss y)_x;$[100h>@z;:[;z];z]]}

/select insert update delete exec  / fkeys[&keys] should be eponymous, e.g. order.customer.nation   
/{keys|cols}`t `f's{xasc|xdesc}`t n!`t xcol(prename) xcols(prearrange)  FT(xcol xasc xdesc)
view:{(2+*x ss"::")_x:*|*|.`. .`\:x};tables:{."\\a ",$$[^x;`;x]};views:{."\\b ",$$[^x;`;x]}
cols:{$[.Q.qp x:.Q.v x;.Q.pf,!+x;98h=@x;!+x;11h=@!x;!x;!+0!x]} /cols:{!.Q.V x}
xcols:{(x,f@&~(f:cols y)in x)#y};keys:{$[98h=@x:.Q.v x;0#`;!+!x]};xkey:{(#x)!.[0!y;();xcols x]};
xcol:{.Q.ft[{+$[99h=@x;@[!y;(!y)?!x;:;. x];x,(#x)_!y]!. y:+y}x]y};xasc:{$[$[#x;~`s=-2!(0!.Q.v y)x;0];.Q.ft[@[;*x;`s#]].Q.ord[<:;x]y;y]};xdesc:{$[#x;.Q.ord[>:;x]y;y]}
fkeys:{(&~^x)#x:.Q.fk'.Q.V x};meta:{([!c].Q.ty't;f:.Q.fk't;a:-2!'t:. c:.Q.V x)}

/ R uj R(union join) R lj K(left(equi/asof)join)   trade asof`sym`time!(`IBM;09:31:00.0)
lj:{$[$[99h=@y;(98h=@!y)&98h=@. y;()~y];.Q.ft[,\:[;y];x];'"type"]} /;la:{$[&/j:z>-1;x,'y z;+.[+ff[x]y;(!+y;j);:;.+y z j:&j]]}{la[x;. y](!y)?(!+!y)#x}[;y]]x} /lj:,\:;aj:{lj[y]`s#xkey[x]z};aj0:{lj[y]`s#(x#z)!z}; /;bn:{@[i;&0>i:x bin y;:;#x]}
ljf:{$[`s=-2!y;ajf[!+!y;x;0!y];.Q.ft[{$[&/j:(#y:. y)>i?:(!+i:!y)#x;.Q.fl[x]y i;+.[+x;(f;j);:;.+.Q.fl[((f:!+y)#x:.Q.ff[x]y)j]y i j:&j]]}[;y]]x]}
.Q.ajf0:{[f;g;x;y;z]x,:();z:0!z;d:$[g;x_z;z];g:(:;^)f;f:(,;^)f;$[&/j:-1<i:(x#z)bin x#y;f'[y;d i];+.[+.Q.ff[y]d;(!+d;j);g;.+d i j:&j]]}
aj:{.Q.ft[.Q.ajf0[0;1;x;;z]]y};aj0:{.Q.ft[.Q.ajf0[0;0;x;;z]]y};ajf:{.Q.ft[.Q.ajf0[1;1;x;;z]]y};ajf0:{.Q.ft[.Q.ajf0[1;0;x;;z]]y}
ij:{.Q.ft[{x[j],'(. y)i j:&(#y)>i:(!y)?(!+!y)#x}[;y]]x}
ijf:{.Q.ft[{.Q.fl[x j]y i j:&(#y:. y)>i?:(!+i:!y)#x}[;y]]x}
pj:{.Q.ft[{x+0i^y(!+!y)#x}[;y]]x};asof:{f:!$[99h=@y;y;+y];(f_x)(f#x)bin y}
uj:{$[()~x;y;()~y;x;98h=@x;x,(!+x:.Q.ff[x;y])#.Q.ff[y;x];lj[(?(!x),!y)#x]y]}
ujf:{$[()~x;y;98h=@x;x,(!+x:.Q.ff[x;y])#.Q.ff[y;x];ljf[(?(!x),!y)#x]y]}

/wj[-1000 2000+\:trade`time;`sym`time;trade;(quote;(max;`ask);(min;`bid))]  (given `sym`time xasc quote)
ww:{[a;w;f;y;z]f,:();e:1_z;z:*z;y,'n#+(:/'f)!+{[e;d;a;b]e .'d@\:\:a+!b-a}[*:'e;z f:1_'e]/'$[n:#*w;+$[#g;(g#z)?g#y;0]|/:a+$[#g:-1_f;(f#z)bin@[f#y;*|f;:;]@;z[*f]bin]'w;,0 0]}
wj:{[w;f;y;z]ww[0 1;w;f;y;z]};wj1:{[w;f;y;z]ww[1;w-1 0;f;y;z]}

fby:{$[(#x 1)=#y;@[(#y)#x[0]0#x 1;g;:;x[0]'x[1]g:.=y];'`length]};xgroup:{x,:();a:x#y:0!y;$[#x_:y;+:'x@=a;a!+f!(#f:!+x)#()]};ungroup:{$[#x:0!x;,/+:'x;x]}
ej:{x,:();y[&#:'i],'(x_z)(!0),/i:(=x#z:0!z)x#y:0!y} /{ungroup lj[z]xgroup[x]y}

/`[:../]t[.{csv|txt}]
save:{$[1=#p:`\:*|`\:x:-1!x;set[x;. *p];   x 0:.h.tx[p 1]@.*p]}'
load:{$[1=#p:`\:*|`\:x:-1!x;set[*p;. x];set[*p].h.xt[p 1]@0:x]}'
rsave:{x:-1!x;.[`/:x,`;();:;.*|`\:x]}'
rload:{x:-1!x;.[*|`\:x;();:;.     x]}'
dsave:{.[*x;1_x,y,`;:;@[;*!+a;`p#].Q.en[*x]a:. y];y}/: 

show:{1 .Q.s x;};csv:"," / ";"  also \z 1 for "D"$"dd/mm/yyyy"

parse:{$["\\"=*x;(system;1_x);-5!x]};eval:-6!;reval:-24!
