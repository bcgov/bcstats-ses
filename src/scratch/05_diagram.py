# If not installed, run: pip install graphviz
from graphviz import Digraph

# dot = Digraph(comment='B.C. Prosperity Index', format='png')
# dot.attr(rankdir='TB', layout='dot')
# dot.attr('node', shape='box', fontname='Helvetica', fontsize='10')

# dot.node('A', 'B.C. Prosperity Index')
# dot.node('B', 'Business Environment')
# dot.node('C', 'Economic Well-Being')
# dot.node('D', 'Societal Well-Being')

# dot.node('B1', 'Labour Productivity')
# dot.node('B2', 'Investment')
# dot.node('B3', 'Innovation')
# dot.node('B4', 'Education')

# dot.node('C1', 'GDP Per Capita')
# dot.node('C2', 'Household Income')
# dot.node('C3', 'Unemployment Rate')
# dot.node('C4', 'Housing Affordability')

# dot.node('D1', 'Life Expectancy')
# dot.node('D2', 'Poverty Rate')
# dot.node('D3', 'Income Inequality')
# dot.node('D4', 'Environment')

# dot.edges([('A','B'), ('A','C'), ('A','D')])
# dot.edges([('B','B1'), ('B','B2'), ('B','B3'), ('B','B4')])
# dot.edges([('C','C1'), ('C','C2'), ('C','C3'), ('C','C4')])
# dot.edges([('D','D1'), ('D','D2'), ('D','D3'), ('D','D4')])

# dot.render('BC_Prosperity_Index', view=True)


# 
# dot = Digraph(comment='B.C. Prosperity Index', format='png')
# dot.attr(rankdir='TB', layout='dot')
# dot.attr('node', shape='box', fontname='Helvetica', fontsize='10')
# 
# # Top-level nodes
# dot.node('A', 'B.C. Prosperity Index')
# dot.node('B', 'Business Environment')
# dot.node('C', 'Economic Well-Being')
# dot.node('D', 'Societal Well-Being')
# 
# # Children of B
# dot.node('B1', 'Labour Productivity')
# dot.node('B2', 'Investment')
# dot.node('B3', 'Innovation')
# dot.node('B4', 'Education')
# 
# # Children of C
# dot.node('C1', 'GDP Per Capita')
# dot.node('C2', 'Household Income')
# dot.node('C3', 'Unemployment Rate')
# dot.node('C4', 'Housing Affordability')
# 
# # Children of D
# dot.node('D1', 'Life Expectancy')
# dot.node('D2', 'Poverty Rate')
# dot.node('D3', 'Income Inequality')
# dot.node('D4', 'Environment')
# 
# # Connect top-level nodes
# dot.edges([('A','B'), ('A','C'), ('A','D')])
# 
# # Connect child nodes under B
# dot.edge('B','B1')
# dot.edge('B','B2')
# dot.edge('B','B3')
# dot.edge('B','B4')
# # Add invisible edges to enforce vertical alignment
# dot.edge('B1','B2', style='invis')
# dot.edge('B2','B3', style='invis')
# dot.edge('B3','B4', style='invis')
# 
# # Connect child nodes under C
# dot.edge('C','C1')
# dot.edge('C','C2')
# dot.edge('C','C3')
# dot.edge('C','C4')
# # Invisible edges for vertical alignment
# dot.edge('C1','C2', style='invis')
# dot.edge('C2','C3', style='invis')
# dot.edge('C3','C4', style='invis')
# 
# # Connect child nodes under D
# dot.edge('D','D1')
# dot.edge('D','D2')
# dot.edge('D','D3')
# dot.edge('D','D4')
# # Invisible edges for vertical alignment
# dot.edge('D1','D2', style='invis')
# dot.edge('D2','D3', style='invis')
# dot.edge('D3','D4', style='invis')
# 
# dot.render('BC_Prosperity_Index', view=False)



from graphviz import Digraph

dot = Digraph('BC_Prosperity_Index', format='png')

# Global graph attributes
dot.attr(
    layout='dot',
    rankdir='TB',
    # splines='ortho',
    nodesep='0.4',
    ranksep='0.4'
)
dot.attr('node', fontname='Helvetica', fontsize='10', shape='box')

# Top-level node
dot.node('A', 'B.C. Prosperity Index')

# --- Cluster B ---
with dot.subgraph(name='cluster_B') as b:
    b.attr(style='invis')
    
    b.node('B', 'Business Environment')
    
    # B children
    b.node('B1', 'Labour Productivity')
    b.node('B2', 'Investment')
    b.node('B3', 'Innovation')
    b.node('B4', 'Education')
    
    # B line nodes (points)
    b.attr('node', shape='point', width='0.01', height='0.01', label='')
    b.node('B_line1')
    b.node('B_line2')
    b.node('B_line3')
    b.node('B_line4')

    # Vertical chain under B
    b.edge('B', 'B_line1', dir='none')
    b.edge('B_line1', 'B_line2', dir='none')
    b.edge('B_line2', 'B_line3', dir='none')
    b.edge('B_line3', 'B_line4', dir='none')

    # Connect line nodes to children horizontally
    b.edge('B_line1', 'B1', constraint='false', dir='none')
    b.edge('B_line2', 'B2', constraint='false', dir='none')
    b.edge('B_line3', 'B3', constraint='false', dir='none')
    b.edge('B_line4', 'B4', constraint='false', dir='none')

    # rank=same subgraphs for B pairs
    with b.subgraph() as s:
        s.attr(rank='same')
        s.node('B_line1')
        s.node('B1')
    with b.subgraph() as s:
        s.attr(rank='same')
        s.node('B_line2')
        s.node('B2')
    with b.subgraph() as s:
        s.attr(rank='same')
        s.node('B_line3')
        s.node('B3')
    with b.subgraph() as s:
        s.attr(rank='same')
        s.node('B_line4')
        s.node('B4')

# --- Cluster C ---
dot.attr('node', shape='box', fontname='Helvetica', fontsize='10')
with dot.subgraph(name='cluster_C') as c:
    c.attr(style='invis')
    
    c.node('C', 'Economic Well-Being')
    c.node('C1', 'GDP Per Capita')
    c.node('C2', 'Household Income')
    c.node('C3', 'Unemployment Rate')
    c.node('C4', 'Housing Affordability')

    c.attr('node', shape='point', width='0.01', height='0.01', label='')
    c.node('C_line1')
    c.node('C_line2')
    c.node('C_line3')
    c.node('C_line4')

    c.edge('C', 'C_line1', dir='none')
    c.edge('C_line1', 'C_line2', dir='none')
    c.edge('C_line2', 'C_line3', dir='none')
    c.edge('C_line3', 'C_line4', dir='none')

    c.edge('C_line1', 'C1', constraint='false', dir='none')
    c.edge('C_line2', 'C2', constraint='false', dir='none')
    c.edge('C_line3', 'C3', constraint='false', dir='none')
    c.edge('C_line4', 'C4', constraint='false', dir='none')

    with c.subgraph() as s:
        s.attr(rank='same')
        s.node('C_line1')
        s.node('C1')
    with c.subgraph() as s:
        s.attr(rank='same')
        s.node('C_line2')
        s.node('C2')
    with c.subgraph() as s:
        s.attr(rank='same')
        s.node('C_line3')
        s.node('C3')
    with c.subgraph() as s:
        s.attr(rank='same')
        s.node('C_line4')
        s.node('C4')

# --- Cluster D ---
dot.attr('node', shape='box', fontname='Helvetica', fontsize='10')
with dot.subgraph(name='cluster_D') as d:
    d.attr(style='invis')
    
    d.node('D', 'Societal Well-Being')
    d.node('D1', 'Life Expectancy')
    d.node('D2', 'Poverty Rate')
    d.node('D3', 'Income Inequality')
    d.node('D4', 'Environment')

    d.attr('node', shape='point', width='0.01', height='0.01', label='')
    d.node('D_line1')
    d.node('D_line2')
    d.node('D_line3')
    d.node('D_line4')

    d.edge('D', 'D_line1', dir='none')
    d.edge('D_line1', 'D_line2', dir='none')
    d.edge('D_line2', 'D_line3', dir='none')
    d.edge('D_line3', 'D_line4', dir='none')

    d.edge('D_line1', 'D1', constraint='false', dir='none')
    d.edge('D_line2', 'D2', constraint='false', dir='none')
    d.edge('D_line3', 'D3', constraint='false', dir='none')
    d.edge('D_line4', 'D4', constraint='false', dir='none')

    with d.subgraph() as s:
        s.attr(rank='same')
        s.node('D_line1')
        s.node('D1')
    with d.subgraph() as s:
        s.attr(rank='same')
        s.node('D_line2')
        s.node('D2')
    with d.subgraph() as s:
        s.attr(rank='same')
        s.node('D_line3')
        s.node('D3')
    with d.subgraph() as s:
        s.attr(rank='same')
        s.node('D_line4')
        s.node('D4')

# Connect A to B, C, D
dot.edge('A', 'B')
dot.edge('A', 'C')
dot.edge('A', 'D')

# Render the graph
dot.render('BC_Prosperity_Index', view=True)

