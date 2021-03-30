\d .Q
a0:(#:;*:;last;sum;prd;min;max;.q.all;.q.any;?:),a1:(avg;wsum;wavg;var;dev;cov;cor),`.q `svar`sdev`scov`med
IN:{$[99h<@x;x in y;0]};qa:{$[qb x;0;IN[*x;a0];1;|/qa'1_x]};qb:{(2>#x)|(@x)&~11=@x}

pt:pm:()                                    / partitioned tables, partitions mapped
vt:(enlist`)!enlist()!()                    / virtual tables
dd:{` sv x,`$string y}                      / join symbols
p1:{                                        / x:table name, y:partition directories, z:partition values
  $[count pm;pm[x](y;z);                      / if partitions have been mapped, return partitions
    z in vt[y;x];vp x;                          / else-if partition values in virtual table dictionary, return virtual partitions 
    flip(key flip value x)!` sv dd[y;z],x]}     / else, return tables of columns mapped to file path
d0:{dd[last pd;last pv]}                    / most recent partition

view:{
  pd::PD x:$[(::)~x;x;where PV in x];             / filter partition directories by partition values intersect values in argument
  u~:?u::..[pf;();:;pv::PV x];                    / filter partition values and partition field variable, set u indicating date-based segmentation
  x{.[y;();:;flip(x . y,`.d)!y]}'pt::key x:d0[];  / list of partitioned tables and table values based on most recent partition
  pn::pt!(count pt)#()}                           / partition counts

L:{D::();if[x~,`par.txt;if[~#x:,/D::!:'P::`$":",'0:*x;'`empty]];if[^*PV::x@:<x:(t:"DMJJ"i:10 7 4?#$*x)$$x;'`part]
 PD::$[#D::t$'$D;,/{P@&x in'D}'?PV;(#PV)#`:.];pf::`date`month`year`int i;view[];if[(0>."\\p")|."\\_";cn'.:'pt];}
cn:{
  $[count n:pn x:value flip x;n;            / if there are entries in the partition count dictionary for the table name, return those values
    pn[x]:(count p1 .)each flip(x;pd;pv)]}    / else, count each partition and append to partition count dictionary
dt:{cn[y]where pv in x}                     / partition counts
fp:{+((,*x)!,(#z)#$[-7h=@y;y;(*|x)$y]),+z}
foo:{[t;c;b;a;v;d]if[v;g:*|`\:b f:*!b;b:1_b];,/$[v|~#a;d fp[$[v;f,g;pf]]';::]p[(.+t;c;b;a)]d}

a2:({(%;(sum;("f"$;x));(sum;(~^:;x)))};{(sum;(*;("f"$;x);y))};{(%;(wsum;x;y);(sum;(*;x;(~^:;y))))};{(cov;x;x)};{(sqrt;(var;x))}
 {(-;(avg;(*;("f"$;x);y));(*;(avg;x);(avg;y)))};{(%;(cov;x;y);(*;(dev;x);(dev;y)))};{(.q.scov;x;x)};{(sqrt;(.q.svar;x))};{(*;(%;(#:;`i);(+;-1;(#:;`i)));(cov;x;y))};{'`part})

qd:{$[(#:)~*x;(?:)~*x 1;0]}
xy:{`$$*&x~/:y}
x1:{$[qb x;();IN[*x;a0];$[qd x;1_x;,x];,/x1'1_x]}
x0:{$[qb x;x;IN[*x;a1];x0 a2[a1?*x]. 1_x;x0'x]}
x2:{$[qb x; x;IN[*x;a0];$[qd x;(#:;(?:;(,/;xy[x 1]y)));[y:xy[x]y;$[(?:)~*x;(?:;(,/;y));(#:)~*x;(sum;y);(*x;y)]]];x2[;y]'x]}
ua:{((`$$!#u)!u;x2[;u:?,/x1'x]'x:x0'x)}


qe:{$[count x;99h=type x;1]} / is dict or empty list

ps:{[t;c;b;a]                                   / partition select
  if[-11h=type t;t:value t];                    / pass by reference
  if[not qe[a] and qe[b] or -1h=type b;'`nyi];  / validate arguments
  d:pv;                                         / partition values
  v:$[q:0>type b;0;                             / if boolean, 0
    not count b;0;                                / else-if empty list, 0
    -11h=type v:first value b;pf~first` vs v;     / else-if first grouping is on partition field, 1
    0];                                           / else, 0
  if[                                           / if..
    $[not count c;0;                              / if empty list, 0
      type first c;0;                               / else-if first constraint is anything other than a list, 0
      -11h=type x:c[0]1;pf~first` vs x;             / else-if first constraint is on partition field, 1
      0];                                           / else, 0
    d@:where eval first c;c:1_c];                 / then filter partition values
  if[                                           / if..
    $[count c;0;                                  / if there are constraints on columns other than the partition field, 0
      (g:(value a)~enlist pf)                       / else, aggregation concerns partition field only.. 
        or(value a)~enlist(count;`i)];                / or count of the virtual column only, 1
    f:key a;                                      / then get the name of the aggregated column
    j:dt[d]t;                                     / then get partition counts
    if[q;:+f!,$[g;?d@&0<j;,+/j]];
    if[v&1=#b;:?[+(pf,f)!(d;j)[;&0<j];();b;f!,(sum;*f)]]];
  if[~#d;d:pv@&pv=*|pv;c:,()];
  f:$[q;0#`;!b];
  g:$[#a;qa@*a;0];
  $[(1=#d)|$[q;~g;u&pf~*. b];
    $[~q;.q.xkey[f];b;?:;::]foo[t;c;b;a;v]d;
    (?).(foo[t;c;$[q;()!();b];*a;v]d;();$[q;0b;f!f];*|a:$[g;ua a;(a;$[#a;(,/;)'k!k:!a;()])])]}
