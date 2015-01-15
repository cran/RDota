\encoding{UTF-8} 
\name{quickpick}
\alias{quickpick}
\title{
Quick pick function for given lineup
}
\description{Quick pick function for given lineup
}
\usage{
quickpick(enemy = c("", "", "", "", ""), teammate = c("", "", "", "", ""))
}
\arguments{
  \item{enemy}{
Vectors that include FULL names of enemies' heros in Simplified Chinese, English or Korean. Different languages are allowed to be mixed. There should be at least one non-empty parameters between enemy and teammate.
}
  \item{teammate}{
Vectors that include FULL names of teammates' heros in Simplified Chinese, English or Korean. Different languages are allowed to be mixed. There should be at least one non-empty parameters between enemy and teammate.
}
}
\details{
The theory behind is trivial to describe. The core assumption is that, there is no three-hero combo. For example, Faceless Void, Skywrath Mage and Kunkka are a mega combo in teamfight, but the function assume that the advantage of this lineup could be explained by FV+Sky, FV+Kunkka and Sky+Kunkka. The data is from \url{dotamax.com}.

\if{html}{\out{For non-English users, the examples are here}}

##\if{html}{\out{由于无法在Example部分使用Unicode，中文和韩文的案例在此：}}

\if{html}{\out{quickpick(enemy="퍼지")}}

##\if{html}{\out{中英文混合可自动产生中文结果)}}

\if{html}{\out{quickpick(enemy=c("主宰","Tiny"),teammate=c("Lina","影魔"))}}
}

\author{
Xiao Lei. Send me email if you have any great idea or you are willing to help.
\email{leixiao322@hotmail.com}
}


\examples{
quickpick(enemy=c("Zeus","Luna"),teammate=c("Axe"))
}
