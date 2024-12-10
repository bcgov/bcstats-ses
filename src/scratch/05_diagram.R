# Install and load DiagrammeR if not already installed
# install.packages("DiagrammeR")

library(DiagrammeR)

grViz("
digraph {
  graph [layout=dot, rankdir=TB, splines=ortho, nodesep=0.2, ranksep=0.4]

  node [fontname=Helvetica, fontsize=10, shape=box]

  // Top-level node
  Index [label='Index', style=filled, fillcolor=\"#5c4bab\", fontcolor=white, penwidth=0.5]

  //////////////////////////////
  // Economic Cluster
  //////////////////////////////
  subgraph cluster_Economic {
    style=invis;
    Economic [label='Economic', shape=box, style=\"rounded,filled\", fillcolor=white]

    // Children of Economic
    E1 [label='Population growth rate']
    E2 [label='Population density']
    E3 [label='Median Income']
    E4 [label='Gini index']
    E5 [label='Proportion employed\nin a management occupation']
    E6 [label='Labor market participation']
    E7 [label='Employment rate']
    E8 [label='House affordability']
    E9 [label='Home ownership/renter']
    E10 [label='Houses needing repaired']
    E11 [label='Number of parks, libraries, and hospitals']
    E12 [label='Remoteness']
    E13 [label='Internet connectivity']

    node [shape=point, width=0.01, height=0.01, label=\"\"]
    E_line1
    E_line2
    E_line3
    E_line4
    E_line5
    E_line6
    E_line7
    E_line8
    E_line9
    E_line10
    E_line11
    E_line12
    E_line13

    Economic -> E_line1 [dir=none]
    E_line1 -> E_line2 [dir=none]
    E_line2 -> E_line3 [dir=none]
    E_line3 -> E_line4 [dir=none]
    E_line4 -> E_line5 [dir=none]
    E_line5 -> E_line6 [dir=none]
    E_line6 -> E_line7 [dir=none]
    E_line7 -> E_line8 [dir=none]
    E_line8 -> E_line9 [dir=none]
    E_line9 -> E_line10 [dir=none]
    E_line10 -> E_line11 [dir=none]
    E_line11 -> E_line12 [dir=none]
    E_line12 -> E_line13 [dir=none]

    E_line1 -> E1 [constraint=false, dir=none]
    { rank=same; E_line1; E1 }

    E_line2 -> E2 [constraint=false, dir=none]
    { rank=same; E_line2; E2 }

    E_line3 -> E3 [constraint=false, dir=none]
    { rank=same; E_line3; E3 }

    E_line4 -> E4 [constraint=false, dir=none]
    { rank=same; E_line4; E4 }

    E_line5 -> E5 [constraint=false, dir=none]
    { rank=same; E_line5; E5 }

    E_line6 -> E6 [constraint=false, dir=none]
    { rank=same; E_line6; E6 }

    E_line7 -> E7 [constraint=false, dir=none]
    { rank=same; E_line7; E7 }

    E_line8 -> E8 [constraint=false, dir=none]
    { rank=same; E_line8; E8 }

    E_line9 -> E9 [constraint=false, dir=none]
    { rank=same; E_line9; E9 }

    E_line10 -> E10 [constraint=false, dir=none]
    { rank=same; E_line10; E10 }

    E_line11 -> E11 [constraint=false, dir=none]
    { rank=same; E_line11; E11 }

    E_line12 -> E12 [constraint=false, dir=none]
    { rank=same; E_line12; E12 }

    E_line13 -> E13 [constraint=false, dir=none]
    { rank=same; E_line13; E13 }
  }

  //////////////////////////////
  // Education Cluster
  //////////////////////////////
  subgraph cluster_Education {
    style=invis;
    Education [label='Education', shape=box, style=\"rounded,filled\", fillcolor=white]

    // Children of Education
    Ed1 [label='Grade 12 on time graduation rate']
    Ed2 [label='FSA 4 and 7 numeracy scores']
    Ed3 [label='FSA 4 and 7 literacy scores']
    Ed4 [label='Math grade 10-12 course marks']
    Ed5 [label='English grade 10-12 course marks']
    Ed6 [label='Student learning survey:\nLike school']
    Ed7 [label='Student learning survey:\nHow many times a week do you eat breakfast']
    Ed8 [label='Proportion of population\nwithout high school diploma']

    node [shape=point, width=0.01, height=0.01, label=\"\"]
    Ed_line1
    Ed_line2
    Ed_line3
    Ed_line4
    Ed_line5
    Ed_line6
    Ed_line7
    Ed_line8

    Education -> Ed_line1 [dir=none]
    Ed_line1 -> Ed_line2 [dir=none]
    Ed_line2 -> Ed_line3 [dir=none]
    Ed_line3 -> Ed_line4 [dir=none]
    Ed_line4 -> Ed_line5 [dir=none]
    Ed_line5 -> Ed_line6 [dir=none]
    Ed_line6 -> Ed_line7 [dir=none]
    Ed_line7 -> Ed_line8 [dir=none]

    Ed_line1 -> Ed1 [constraint=false, dir=none]
    { rank=same; Ed_line1; Ed1 }

    Ed_line2 -> Ed2 [constraint=false, dir=none]
    { rank=same; Ed_line2; Ed2 }

    Ed_line3 -> Ed3 [constraint=false, dir=none]
    { rank=same; Ed_line3; Ed3 }

    Ed_line4 -> Ed4 [constraint=false, dir=none]
    { rank=same; Ed_line4; Ed4 }

    Ed_line5 -> Ed5 [constraint=false, dir=none]
    { rank=same; Ed_line5; Ed5 }

    Ed_line6 -> Ed6 [constraint=false, dir=none]
    { rank=same; Ed_line6; Ed6 }

    Ed_line7 -> Ed7 [constraint=false, dir=none]
    { rank=same; Ed_line7; Ed7 }

    Ed_line8 -> Ed8 [constraint=false, dir=none]
    { rank=same; Ed_line8; Ed8 }
  }

  //////////////////////////////
  // Health Cluster
  //////////////////////////////
  subgraph cluster_Health {
    style=invis;
    Health [label='Health', shape=box, style=\"rounded,filled\", fillcolor=white]

    // Children of Health
    H1 [label='Pharmacare beneficiaries:\nproportion']
    H2 [label='Chronic Disease Registry:\nprevalence of physical chronic health conditions']
    H3 [label='Chronic Disease Registry:\nmood/anxiety']
    H4 [label='Chronic Disease Registry:\nhypertension']
    H5 [label='Chronic Disease Registry:\nasthma']
    H6 [label='Chronic Disease Registry:\nosteoarthritis']
    H7 [label='Chronic Disease Registry:\ndiabetes']
    H8 [label='Vital Events and Statistics:\nInfant mortality rate']
    H9 [label='Vital Events and Statistics:\nlife expectancy']
    H10 [label='Vital Events and Statistics:\nrate of overdose']
    H11 [label='Vital Events and Statistics:\nlone parent at birth']

    node [shape=point, width=0.01, height=0.01, label=\"\"]
    H_line1
    H_line2
    H_line3
    H_line4
    H_line5
    H_line6
    H_line7
    H_line8
    H_line9
    H_line10
    H_line11

    Health -> H_line1 [dir=none]
    H_line1 -> H_line2 [dir=none]
    H_line2 -> H_line3 [dir=none]
    H_line3 -> H_line4 [dir=none]
    H_line4 -> H_line5 [dir=none]
    H_line5 -> H_line6 [dir=none]
    H_line6 -> H_line7 [dir=none]
    H_line7 -> H_line8 [dir=none]
    H_line8 -> H_line9 [dir=none]
    H_line9 -> H_line10 [dir=none]
    H_line10 -> H_line11 [dir=none]

    H_line1 -> H1 [constraint=false, dir=none]
    { rank=same; H_line1; H1 }

    H_line2 -> H2 [constraint=false, dir=none]
    { rank=same; H_line2; H2 }

    H_line3 -> H3 [constraint=false, dir=none]
    { rank=same; H_line3; H3 }

    H_line4 -> H4 [constraint=false, dir=none]
    { rank=same; H_line4; H4 }

    H_line5 -> H5 [constraint=false, dir=none]
    { rank=same; H_line5; H5 }

    H_line6 -> H6 [constraint=false, dir=none]
    { rank=same; H_line6; H6 }

    H_line7 -> H7 [constraint=false, dir=none]
    { rank=same; H_line7; H7 }

    H_line8 -> H8 [constraint=false, dir=none]
    { rank=same; H_line8; H8 }

    H_line9 -> H9 [constraint=false, dir=none]
    { rank=same; H_line9; H9 }

    H_line10 -> H10 [constraint=false, dir=none]
    { rank=same; H_line10; H10 }

    H_line11 -> H11 [constraint=false, dir=none]
    { rank=same; H_line11; H11 }
  }

  //////////////////////////////
  // Social Development Cluster
  //////////////////////////////
  subgraph cluster_Social {
    style=invis;
    Social [label='Social Development', shape=box, style=\"rounded,filled\", fillcolor=white]

    // Children of Social
    S1 [label='Child Welfare Program:\nproportion of children in care']
    S2 [label='Proportion of lone parents']
    S3 [label='Dependency ratio:\n(youth (0 to 19) + seniors (65+)\nper 100 workers (20 to 64))']
    S4 [label='Crime rate: \ntotal persons charged per 100,000 population\naged 12 years and over']
    S5 [label='Proportion of population\non income assistance']
    S6 [label='Proportion of population\non disability assistance']
    S7 [label='Geographic area affected\nby wildfires']

    node [shape=point, width=0.01, height=0.01, label=\"\"]
    S_line1
    S_line2
    S_line3
    S_line4
    S_line5
    S_line6
    S_line7

    Social -> S_line1 [dir=none]
    S_line1 -> S_line2 [dir=none]
    S_line2 -> S_line3 [dir=none]
    S_line3 -> S_line4 [dir=none]
    S_line4 -> S_line5 [dir=none]
    S_line5 -> S_line6 [dir=none]
    S_line6 -> S_line7 [dir=none]

    S_line1 -> S1 [constraint=false, dir=none]
    { rank=same; S_line1; S1 }

    S_line2 -> S2 [constraint=false, dir=none]
    { rank=same; S_line2; S2 }

    S_line3 -> S3 [constraint=false, dir=none]
    { rank=same; S_line3; S3 }

    S_line4 -> S4 [constraint=false, dir=none]
    { rank=same; S_line4; S4 }

    S_line5 -> S5 [constraint=false, dir=none]
    { rank=same; S_line5; S5 }

    S_line6 -> S6 [constraint=false, dir=none]
    { rank=same; S_line6; S6 }

    S_line7 -> S7 [constraint=false, dir=none]
    { rank=same; S_line7; S7 }
  }

  // Connect top-level Index to each main category
  Index -> Economic
  Index -> Education
  Index -> Health
  Index -> Social
}
")
# 
# 
# grViz("
# digraph {
#   graph [layout=dot, rankdir=TB, nodesep=0.2, ranksep=0.4]
#   node [fontname=Helvetica, fontsize=10, shape=box]
# 
#   A [label='B.C. Prosperity Index']
# 
#   # Cluster for B side
#   subgraph cluster_B {
#     style=invis;
#     B [label='Business Environment']
# 
#     # B children
#     B1 [label='Labour Productivity']
#     B2 [label='Investment']
#     B3 [label='Innovation']
#     B4 [label='Education']
# 
#     # B line nodes
#     node [shape=point, width=0.01, height=0.01, label=\"\"]
#     B_line1
#     B_line2
#     B_line3
#     B_line4
# 
#     B -> B_line1 [dir=none]
#     B_line1 -> B_line2 [dir=none]
#     B_line2 -> B_line3 [dir=none]
#     B_line3 -> B_line4 [dir=none]
# 
#     B_line1 -> B1
#     { rank=same; B_line1; B1 }
# 
#     B_line2 -> B2
#     { rank=same; B_line2; B2 }
# 
#     B_line3 -> B3
#     { rank=same; B_line3; B3 }
# 
#     B_line4 -> B4
#     { rank=same; B_line4; B4 }
#   }
# 
#   # Restore node defaults for C cluster
#   node [shape=box, fontname=Helvetica, fontsize=10]
# 
#   # Cluster for C side
#   subgraph cluster_C {
#     style=invis;
#     C [label='Economic Well-Being']
# 
#     # C children
#     C1 [label='GDP Per Capita']
#     C2 [label='Household Income']
#     C3 [label='Unemployment Rate']
#     C4 [label='Housing Affordability']
# 
#     # C line nodes
#     node [shape=point, width=0.01, height=0.01, label=\"\"]
#     C_line1
#     C_line2
#     C_line3
#     C_line4
# 
#     C -> C_line1 [dir=none]
#     C_line1 -> C_line2 [dir=none]
#     C_line2 -> C_line3 [dir=none]
#     C_line3 -> C_line4 [dir=none]
# 
#     C_line1 -> C1
#     { rank=same; C_line1; C1 }
# 
#     C_line2 -> C2
#     { rank=same; C_line2; C2 }
# 
#     C_line3 -> C3
#     { rank=same; C_line3; C3 }
# 
#     C_line4 -> C4
#     { rank=same; C_line4; C4 }
#   }
# 
#   # Restore node defaults for D cluster
#   node [shape=box, fontname=Helvetica, fontsize=10]
# 
#   # Cluster for D side
#   subgraph cluster_D {
#     style=invis;
#     D [label='Societal Well-Being']
# 
#     # D children
#     D1 [label='Life Expectancy']
#     D2 [label='Poverty Rate']
#     D3 [label='Income Inequality']
#     D4 [label='Environment']
# 
#     # D line nodes
#     node [shape=point, width=0.01, height=0.01, label=\"\"]
#     D_line1
#     D_line2
#     D_line3
#     D_line4
# 
#     D -> D_line1 [dir=none]
#     D_line1 -> D_line2 [dir=none]
#     D_line2 -> D_line3 [dir=none]
#     D_line3 -> D_line4 [dir=none]
# 
#     D_line1 -> D1
#     { rank=same; D_line1; D1 }
# 
#     D_line2 -> D2
#     { rank=same; D_line2; D2 }
# 
#     D_line3 -> D3
#     { rank=same; D_line3; D3 }
# 
#     D_line4 -> D4
#     { rank=same; D_line4; D4 }
#   }
# 
#   # Connect A to each group head
#   A -> B
#   A -> C
#   A -> D
# }
# ")
# 
# 
# library(DiagrammeR)
# # The splines=ortho attribute still applies globally, keeping edges orthogonal.
# grViz("
# digraph {
#   graph [layout=dot, rankdir=TB, nodesep=0.8, ranksep=0.2, splines=ortho]
#   node [fontname=Helvetica, fontsize=10, shape=box]
# 
#   A [label='B.C. Prosperity Index']
# 
#   # Cluster for B side
#   subgraph cluster_B {
#     style=invis;
#     B [label='Business Environment']
# 
#     # B children
#     B1 [label='Labour\nProductivity']
#     B2 [label='Investment']
#     B3 [label='Innovation']
#     B4 [label='Education']
# 
#     # B line nodes
#     node [shape=point, width=0.01, height=0.01, label=\"\"]
#     B_line1
#     B_line2
#     B_line3
#     B_line4
# 
#     # Strictly vertical chain for B
#     # No 'constraint=false' on these vertical edges, and dir=none removes arrowheads
#     B -> B_line1 [dir=none]
#     B_line1 -> B_line2 [dir=none]
#     B_line2 -> B_line3 [dir=none]
#     B_line3 -> B_line4 [dir=none]
# 
#     # Connect line nodes to children horizontally
#     # These edges are constraint=false so they don't shift the vertical alignment
#     B_line1 -> B1 [constraint=false]
#     { rank=same; B_line1; B1 }
# 
#     B_line2 -> B2 [constraint=false]
#     { rank=same; B_line2; B2 }
# 
#     B_line3 -> B3 [constraint=false]
#     { rank=same; B_line3; B3 }
# 
#     B_line4 -> B4 [constraint=false]
#     { rank=same; B_line4; B4 }
#   }
# 
#   # C side
#   node [shape=box, fontname=Helvetica, fontsize=10]
#   subgraph cluster_C {
#     style=invis;
#     C [label='Economic Well-Being']
# 
#     C1 [label='GDP Per Capita']
#     C2 [label='Household\nIncome']
#     C3 [label='Unemployment\nRate']
#     C4 [label='Housing\nAffordability']
# 
#     node [shape=point, width=0.01, height=0.01, label=\"\"]
#     C_line1
#     C_line2
#     C_line3
#     C_line4
# 
#     C -> C_line1 [dir=none]
#     C_line1 -> C_line2 [dir=none]
#     C_line2 -> C_line3 [dir=none]
#     C_line3 -> C_line4 [dir=none]
# 
#     C_line1 -> C1 [constraint=false]
#     { rank=same; C_line1; C1 }
# 
#     C_line2 -> C2 [constraint=false]
#     { rank=same; C_line2; C2 }
# 
#     C_line3 -> C3 [constraint=false]
#     { rank=same; C_line3; C3 }
# 
#     C_line4 -> C4 [constraint=false]
#     { rank=same; C_line4; C4 }
#   }
# 
#   # # D side
#   # node [shape=box, fontname=Helvetica, fontsize=10]
#   # subgraph cluster_D {
#   #   style=invis;
#   #   D [label='Societal Well-Being']
#   # 
#   #   D1 [label='Life Expectancy']
#   #   D2 [label='Poverty Rate']
#   #   D3 [label='Income\nInequality']
#   #   D4 [label='Environment']
#   # 
#   #   node [shape=point, width=0.01, height=0.01, label=\"\"]
#   #   D_line1
#   #   D_line2
#   #   D_line3
#   #   D_line4
#   # 
#   #   D -> D_line1 [dir=none]
#   #   D_line1 -> D_line2 [dir=none]
#   #   D_line2 -> D_line3 [dir=none]
#   #   D_line3 -> D_line4 [dir=none]
#   # 
#   #   D_line1 -> D1 [constraint=false]
#   #   { rank=same; D_line1; D1 }
#   # 
#   #   D_line2 -> D2 [constraint=false]
#   #   { rank=same; D_line2; D2 }
#   # 
#   #   D_line3 -> D3 [constraint=false]
#   #   { rank=same; D_line3; D3 }
#   # 
#   #   D_line4 -> D4 [constraint=false]
#   #   { rank=same; D_line4; D4 }
#   # }
# 
#   A -> B
#   A -> C
#   # A -> D
# }
# ")



