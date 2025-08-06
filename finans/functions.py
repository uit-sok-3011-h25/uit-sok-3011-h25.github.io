import nbformat
executed_notebooks = []
d = {}
def calc_notebook(notebook_path, ques):
	if notebook_path in executed_notebooks:
		return d
	executed_notebooks.append(notebook_path)
	
	with open(notebook_path, encoding="utf-8") as f:
		notebook = nbformat.read(f, as_version=4)
	for c in  notebook.cells:
		if c.cell_type == "code":
			if sum(q in c.source for q in ques):
				exec(c.source, globals(), d)
	return d



calc_notebook("3-lecture_optport.ipynb", 
			  ["read_pickle","get_matrix", "cov_matrix", "A =", 
	  		   "portfolio_front", "ax.plot"])