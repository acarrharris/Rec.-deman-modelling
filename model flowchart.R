install.packages('DiagrammeR')
library(DiagrammeR)






grViz(diagram = "digraph flowchart {
  node [fontname = arial, shape = box]
  graph[splines=ortho, overlap = false]
  edge[tailclip = true, headclip = true]

  tab1 [label = '@@1']
  tab2 [label = '@@2']
  tab3 [label = '@@3']
  tab4 [label = '@@4']
  tab6 [label = '@@6']
  tab7 [label = '@@7']
  tab8 [label = '@@8']
  tab9 [label = '@@8']
  tab10 [label = '@@10']
  tab11 [label = '@@11']
  tab13 [label = '@@13']
  tab14 [label = '@@14']
  tab15 [label = '@@15']

  node [fontname = arial, shape = diamond]
  tab5 [label = '@@5']
  tab9 [label = '@@9']
  tab12 [label = '@@12']

  tab1 -> tab2 -> tab3 ->tab4 -> tab5;
  tab5 -> tab6[label='Yes', fontname=arial];
  tab5 -> tab7[label='No', fontname=arial];
  tab6 -> tab8;
  tab7 -> tab8;
  tab8 -> tab9;
  tab9 -> tab10[label='Yes', fontname=arial];
  tab10 -> tab5;
  tab9 -> tab11[label='No', fontname=arial];
  tab11 -> tab12[tailport = 's'];
  tab12 -> tab3[label='Yes', fontname=arial];
  tab12 -> tab13[label='No', fontname=arial];
  tab13 -> tab14;
  tab14 -> tab15;

  {rank=same ; tab9  tab10 };
    {rank=same ; tab9  tab10 };


}
  
  [1]: 'Begin choice occasion iteration'
  [2]: 'Draw trip cost from 2017 marine angler expenditure survey' 
  [3]: 'Draw target number of fluke caught from MRIP catch-per-trip distribution' 
  [4]: 'Assign first fluke caught a value d drawn from D~U[0,1]'
  [5]: 'Is d>d*?'
  [6]: 'Add 1 to keep'
  [7]: 'Add 1 to release'
  [8]: 'Add 1 to catch'
  [9]: 'Is keep<bag limit?'
  [10]: 'Assign next fluke caught a value d drawn from D~U[0,1]'
  [11]: 'Compute the utility using model in Table 2'
  [12]: 'Is repetition<10?'
  [13]: 'Compute the average utility of the trip over the 10 repetitions'
  [14]: 'Compute the probability of taking the trip'
  [15]: 'End choice occasion iteration'

  
  ")







