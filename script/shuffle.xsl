<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:sf="https://ldbeth.sdf.org/shuffle"
                exclude-result-prefixes="xs sf"
                version="3.0">
  <xsl:param name="variant-requirement" select="2"/>
  <xsl:param name="aspect-requirement" select="'*'"/>

  <xsl:output method="text"/>
  <xsl:function name="sf:is-required-variant" as="xs:boolean">
    <xsl:param name="variant" as="xs:integer"/>
    <xsl:sequence select="if (string($variant-requirement) = '*')
                          then true()
                          else (xs:integer($variant-requirement) >= $variant)"/>
  </xsl:function>

  <xsl:template match="/">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="sf:shuffle">
    <xsl:apply-templates select="sf:chunk"/>
  </xsl:template>

  <xsl:template match="sf:chunk[@variant]">
    <xsl:if test="sf:is-required-variant(xs:integer(@variant))">
      <xsl:apply-templates mode="chunk-content"/>
    </xsl:if>
  </xsl:template>
  <!-- Ignore chunks without variant -->
  <xsl:template match="sf:chunk[not(@variant)]"/>

  <xsl:template match="text()" mode="chunk-content">
    <xsl:copy/>
  </xsl:template>

  <xsl:template match="sf:ref" mode="chunk-content">
    <xsl:apply-templates mode="chunk-content"
                         select="/sf:shuffle/sf:chunk[@xml:id = current()/@target]"/>
  </xsl:template>

  <xsl:template match="sf:choose" mode="chunk-content">
    <xsl:apply-templates mode="chunk-content"
                         select="(sf:chunk[sf:is-required-variant(xs:integer(@variant))])[1]"/>
  </xsl:template>
</xsl:stylesheet>
