<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform
    version="3.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:s="http://purl.oclc.org/dsdl/schematron"
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    exclude-result-prefixes="s rng">

  <xsl:output method="xml" omit-xml-declaration="no" indent="yes" standalone="yes"/>
  <xsl:strip-space elements="*"/>

  <xsl:template match="/rng:grammar | /rng:element">
		<!-- call the schema definition template ... -->
		<xsl:call-template name="gatherSchema">
		  <!-- ... with current node as the $schemas parameter ... -->
		  <xsl:with-param name="schemas" select="."/>
		  <!-- ... and any includes in the $include parameter -->
		  <xsl:with-param name="includes" select="document(/rng:grammar/rng:include/@href
                                                  | //rng:externalRef/@href)"/>
		</xsl:call-template>
  </xsl:template>

  <xsl:template name="gatherSchema">
	<xsl:param name="schemas"/>
	<xsl:param name="includes"/>
	<xsl:choose>
	  <xsl:when test="count($schemas) lt count($schemas | $includes)">
		<xsl:call-template name="gatherSchema">
		  <xsl:with-param name="schemas" select="$schemas | $includes"/>
		  <xsl:with-param name="includes" select="document($includes/rng:grammar/rng:include/@href
| $includes//rng:externalRef/@href)"/>
		</xsl:call-template>
	  </xsl:when>
	  <xsl:otherwise>
		<xsl:call-template name="output">
		  <xsl:with-param name="schemas" select="$schemas"/>
		</xsl:call-template>
	  </xsl:otherwise>
	</xsl:choose>
  </xsl:template>

  <xsl:template name="output">
	<xsl:param name="schemas"/>
	<s:schema queryBinding="xslt3">
	  <xsl:for-each select="$schemas//s:ns">
		<xsl:if test="generate-id(.) = generate-id($schemas//s:ns[@prefix = current()/@prefix][1])">
		  <xsl:apply-templates select="." mode="sch"/>
		</xsl:if>
	  </xsl:for-each>
	  <xsl:apply-templates select="$schemas//s:phase" mode="sch"/>
	  <xsl:apply-templates select="$schemas//s:pattern" mode="sch"/>
      <xsl:where-populated>
	    <s:diagnostics>
		  <xsl:apply-templates select="$schemas//s:diagnostics/*" mode="sch"/>
	    </s:diagnostics>
      </xsl:where-populated>
	</s:schema>
  </xsl:template>

  <xsl:mode name="sch" on-no-match="shallow-copy"/>
  <xsl:template mode="sch" match="text()">
    <xsl:value-of select="normalize-space()"/>
  </xsl:template>

</xsl:transform>
