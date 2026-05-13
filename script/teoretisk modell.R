library(DiagrammeR)

grViz("
digraph teori {
  
  graph [layout = neato, overlap = false]
  
  # Felles node-stil (LYS BLÅ ✨)
  node [fontname = Helvetica, style = filled, fillcolor = '#EEF3FC', color = 'grey40']
  
  # Bokser
  ulikheter [label = 'Sosiale & geografiske\nulikheter (klasse, inntekt,\nbolig, m.m.)',
             shape = box, width = 2.3, height = 0.9]
  
  representasjon [label = 'Geografisk representasjon\npå partilister (bydel/\ntoppplassering)',
                  shape = box, width = 2.3, height = 0.9]
  
  tilhorighet [label = 'Opplevd tilhørighet /\nlegitimitet / synlighet',
               shape = box, width = 2.0, height = 0.8]
  
  valgdeltakelse [label = 'Valgdeltakelse\n(bydelsnivå)',
                  shape = box, width = 1.7, height = 0.8]
  
  # Sirkler
  utdanning [label = 'Utdanningsnivå\n(bydel)',
             shape = ellipse, width = 1.8, height = 0.8]
  
  innvandrerandel [label = 'Innvandrerandel\n(bydel)',
                   shape = ellipse, width = 1.8, height = 0.8]
  
  inntekt [label = 'Inntekt\n(bydel)',
           shape = ellipse, width = 1.6, height = 0.8]
  
  # Posisjoner
  ulikheter      [pos = '1,3!']
  representasjon [pos = '4,3!']
  tilhorighet    [pos = '7,3!']
  valgdeltakelse [pos = '10,2!']
  utdanning      [pos = '2,1!']
  innvandrerandel[pos = '1,-1!']
  inntekt        [pos = '8,-1!']
  
  # Hovedpiler
  ulikheter -> representasjon [color = royalblue, penwidth = 2]
  representasjon -> tilhorighet [color = royalblue, penwidth = 2]
  tilhorighet -> valgdeltakelse [color = royalblue, penwidth = 2]
  
  # Direkte/stiplede sammenhenger
  representasjon -> valgdeltakelse [style = dashed, color = royalblue]
  utdanning -> valgdeltakelse [style = dotted, color = grey60]
  innvandrerandel -> valgdeltakelse [style = dotted, color = grey60]
  inntekt -> valgdeltakelse [style = dotted, color = grey60]
}
")
