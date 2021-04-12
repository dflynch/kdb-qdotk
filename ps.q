\d .Q
a0:(count;first;last;sum;prd;min;max;all;any;distinct),a1:(avg;wsum;wavg;var;dev;cov;cor;svar;sdev;scov;med)
IN:{$[99h<type x;x in y;0]}                 / if x is a function, is it in the list of aggregate functions
qa:{$[qb x;0;                               / if there's no aggregation to be done, 0
  IN[first x;a0];1;                           / else-if aggregate function, 1 
  max qa each 1_x]}                           / else, recurse into parse tree looking for aggregate functions
qb:{(2>count x)or(type x)and not 11=type x} / single value or non-symbol list

pt:pm:()                                    / partitioned tables, partitions mapped
vt:(enlist`)!enlist()!()                    / virtual tables
dd:{` sv x,`$string y}                      / join symbols
p1:{                                        / x:table name, y:partition directories, z:partition values
  $[count pm;pm[x](y;z);                      / if partitions have been mapped, return partitions
    z in vt[y;x];vp x;                          / else-if partition values in virtual table dictionary, return virtual partitions 
    flip(key flip value x)!` sv dd[y;z],x]}     / else, return tables of columns mapped to file path
d0:{dd[last pd;last pv]}                    / most recent partition

p:{$[~#D;p2[x;`:.]':y;(,/p2[x]'/':P[i](;)'y)@<,/y@:i:&0<#:'y:D{x@&x in y}\:y]}
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
fp:{flip((enlist first x)!enlist(count z)#$[-7h=type y;y;(last x)$y]),flip z} / prepend virtual column 
foo:{[t;c;b;a;v;d]              / t:table value, c:constraint, b:by, a:aggregation, v:is first grouping on partition field, d:partitions
  if[v;                         / if first grouping is on the partition field
    g:last` vs b f:first key b;   / then get partition field name and display name
    b:1_b];                       / then drop first grouping
  raze $[v or not count a;        / if the first grouping is on the partition field or there are no aggregations
    fp[$[v;f,g;pf]]'[d;];           / then prepend the virtual column 
    ::]
    p[(value flip t;c;b;a)]d}

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
    d@:where eval first c;                        / then filter partition values
    c:1_c];                                       / then drop first constraint
  if[                                           / if..
    $[count c;0;                                  / if there are constraints on columns other than the partition field, 0
      (g:(value a)~enlist pf)                       / else, aggregation concerns partition field only.. 
        or(value a)~enlist(count;`i)];                / or count of the virtual column only, 1
    f:key a;                                      / then get the name of the aggregated column
    j:dt[d]t;                                     / then get partition counts
    if[q;:flip f!enlist                           / then if boolean grouping
      $[g;distinct d where 0<j;                     / if aggregation concerns partition field, partition values with counts greater than zero
        enlist sum j]];                               / else, sum of all partition counts
    if[v and 1=count b;                           / if the one and only grouping is on the partition field
      :?[flip(pf,f)!(d;j)[;where 0<j];();b;f!enlist(sum;first f)]]]; / then select partition counts (note: sementation may not be date-based, hence sum)
  if[not count d;                               / if there are no partitions to select from
    d:pv where pv=last pv;                        / then set 'partition values' to the last partition (not sure why it's not d:last pv)
    c:enlist()];                                  / then drop remaining constraints
  f:$[q;0#`;key b];                             / groupings
  g:$[count a;qa first a;0];                    / aggregate function on first column (why not check each?)
  $[(1=count d)                                 / if one partition to query
    or $[q;not g;                                 / or if boolean grouping and aggregate function not on first column
      u and pf~first value b];                      / or if date-based segmentation and first grouping on partition field
    $[~q;.q.xkey[f];b;?:;::]foo[t;c;b;a;v]d;
    (?).(foo[t;c;$[q;()!();b];*a;v]d;();$[q;0b;f!f];*|a:$[g;ua a;(a;$[#a;(,/;)'k!k:!a;()])])]}
