<TeXmacs|2.1.4>

<style|<tuple|article|british>>

<\body>
  <doc-data|<doc-title|Notes on Kleinian Groups>|<doc-author|<author-data|<author-name|Simon
  Beaumont>|<\author-affiliation>
    Autumn 2025
  </author-affiliation>>>>

  <abstract-data|<\abstract>
    Some notes in the course of reading and coding up Grandma's recipes from
    the book Indra's Pearls by Mumford, Series and Wright<cite|mumford2002>.
  </abstract>>

  <section|Möbius transformations>

  A Möbius transformation is specified by four complex parameters,

  <\equation*>
    T z=<dfrac|a*z+b|c*z+d>
  </equation*>

  They have the extraordinary feature that they map the set of all circles
  and straight lines onto the set of all circles and straight lines, known
  collectively as <em|clines>. Möbius transformations also preserve angles
  and their orientations, that is to say they are <em|conformal> maps, whilst
  freely distorting other shapes.

  <section|The Riemann Sphere and <name|<name|<samp|<name|<math|<wide|\<bbb-C\>|^>>>>>>>

  \ The extended complex plane and its model the <em|Riemann
  sphere><\footnote>
    Named after the great German mathematician Bernhard Riemann (1826-1866)
    who among many other profound contributions to mathematics made
    pioneering contributions to differential geometry and created the field
    of Riemannian geometry with impacts on group theory, analysis and
    algebraic and differential topology.\ 
  </footnote> is the complex plane with one point at infinity. The extended
  plane represents the extended complex numbers

  <\equation*>
    <wide|\<bbb-C\>|^>=\<bbb-C\>\<cup\><around*|{|\<infty\>|}>
  </equation*>

  It worth noting that there is only one such point, as <em|complex infinity>
  has infinite magnitude but undefined phase; having undefined phase is more
  obvious in the case of complex zero. Any rational function with complex
  coefficients can be extended to a holomorphic function on the Riemann
  sphere. The extended complex numbers have extended arithmetic but do not
  form a <em|field> due to not having additive or multiplicative inverses,
  however the following operations are defined:

  \;

  <tabular*|<tformat|<twith|table-lborder|1n>|<twith|table-rborder|1n>|<twith|table-bborder|1n>|<twith|table-tborder|1n>|<cwith|1|-1|1|-1|cell-tborder|1ln>|<cwith|1|-1|1|-1|cell-bborder|1ln>|<cwith|1|-1|1|-1|cell-lborder|1ln>|<cwith|1|-1|1|-1|cell-rborder|1ln>|<table|<row|<cell|<math|for
  all nonzero z>>|<cell|<math|z+\<infty\>=\<infty\>>>|<cell|<math|z\<times\>\<infty\>=\<infty\>>>|<cell|<math|\<infty\>\<times\>\<infty\>=\<infty\>>>|<cell|<math|z/0=\<infty\>>>|<cell|<math|z/\<infty\>=\<infty\>>>|<cell|<math|\<infty\>/0=0>>|<cell|<math|0/\<infty\>=0>>>|<row|<cell|undefined>|<cell|<math|\<infty\>+\<infty\>>>|<cell|<math|\<infty\>-\<infty\>>>|<cell|<math|0\<times\>\<infty\>>>|<cell|<math|0/0>>|<cell|<math|\<infty\>/\<infty\>>>|<cell|>|<cell|>>>>>

  \;

  \ The complex plane is mapped to the unit sphere with its south pole at the
  origin and it's north pole represents the point at inifinity. The points on
  the complex plane are mapped to the sphere by a ray from the point at to
  the north pole to where this intersects the surface of the sphere; this is
  a complex projective space and can be thought of as the complex projective
  line <math|\<b-P\><rsup|1><around*|(|\<b-C\>|)>>.

  Among those rules some other equations regarding the value of a
  transformation at infinity is give on page 70, the authors omit the real
  story here and the relevant equation should have been stated:

  <\equation*>
    T<around*|(|\<infty\>|)>=<below|lim|z\<rightarrow\>\<infty\>><suppressed|<explicit-space>><dfrac|a*z+b|c*z+d>=<below|lim|z\<rightarrow\>\<infty\>><suppressed|<explicit-space>><dfrac|a+b/\<infty\>|c+d/\<infty\>>=<frac|a|c>
  </equation*>

  <section|The Kleinian Group>

  It turns out that the group of automorphism of the Riemann sphere, (which
  are invertible conformal maps), are exactly the Möbius transformations with
  non-zero determinants. The Möbius transformations are <em|homographies> of
  the complex projective line. Two matrices yield the same transformation iff
  they differ by a non-zero factor. The group of Möbius transformations is
  the <em|projective linear group> <math|PGL<around*|(|2,C|)>>. The Kleinian
  group is a discrete subgroup of the group of orienteation preserving
  isometries of hyperbolic 3-space <math|\<b-H\><rsup|3>>, this is
  identifiable with <math|PSL<around*|(|2,C|)>> which is the quotient group
  of the 2 by 2 matrices of determinant 1 by their <em|centre>. Thus a
  Kleinian group can be defined as a subgroup <math|\<Gamma\>> of
  <math|PGL<around*|(|2,C|)>> acting on one of these spaces.

  \;

  \ 

  <\bibliography|bib|tm-plain|>
    <\bib-list|1>
      <bibitem*|1><label|bib-mumford2002>David Mumford, Caroline
      Series<localize|, and >David Wright.
      <newblock><with|font-shape|italic|Indra's Pearls: The Vision of Felix
      Klein>. <newblock>Cambridge University Press, 2002.<newblock>
    </bib-list>
  </bibliography>
</body>

<initial|<\collection>
</collection>>

<\attachments>
  <\collection>
    <\associate|bib-bibliography>
      <\db-entry|+2y839HIiFxA|book|mumford2002>
        <db-field|contributor|root>

        <db-field|modus|manual>

        <db-field|newer|+2y839HIiFx9>

        <db-field|date|1758712273>
      <|db-entry>
        <db-field|author|David <name|Mumford><name-sep>Caroline
        <name|Series><name-sep>David <name|Wright>>

        <db-field|title|Indra's Pearls: The Vision of Felix Klein>

        <db-field|publisher|Cambridge University Press>

        <db-field|year|2002>

        <db-field|place|Cambridge>
      </db-entry>
    </associate>
  </collection>
</attachments>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|1>>
    <associate|auto-2|<tuple|2|1>>
    <associate|auto-3|<tuple|3|2>>
    <associate|auto-4|<tuple|3|2>>
    <associate|bib-mumford2002|<tuple|1|2>>
    <associate|footnote-1|<tuple|1|1>>
    <associate|footnr-1|<tuple|1|1>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|bib>
      mumford2002
    </associate>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>Möbius
      transformations> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>The
      Riemann Sphere and <with|font-shape|<quote|small-caps>|<with|font-shape|<quote|small-caps>|<with|font-family|<quote|ss>|<with|font-shape|<quote|small-caps>|<with|mode|<quote|math>|<wide|\<bbb-C\>|^>>>>>>>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|3<space|2spc>The
      Kleinian Group> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-3><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|Bibliography>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-4><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>