\name{mapping_data}
\alias{mapping_data}
\title{Convert Data to Other Corresponding Value}
\description{
}
\usage{
mapping_data(data=NULL, col_name=NULL, ori_tag=NULL, mapping_tag=NULL, other_tag=NULL)
}
\arguments{
\item{data}{data.frame: your input genomic data.}
\item{col_name}{character: column name.}
\item{ori_tag}{vector: a list of values contained in your input genomic data.}
\item{mapping_tag}{vector: a list of replaced values.}
\item{other_tag}{any basic data type or NULL (default is NULL): values contained in ori_tag but not in your input data.}
}
\examples{
# In data frame 'data_gene', all the value 'CP003879.1(character)' in column 'SeqID' will be replaced by '1(numeric)'. 
data_gene <- mapping_data(data=data_gene, col_name="SeqID", ori_tag=c("CP003879.1"), mapping_tag=c(1))

# In data frame 'data_gene', all the value in COGcolor will be replaced. 
data_gene <- mapping_data(data=data_gene, col_name="COGcolor", ori_tag=cog_color$categories, mapping_tag=cog_color$hex, other_tag="#BBBBBB")
}
