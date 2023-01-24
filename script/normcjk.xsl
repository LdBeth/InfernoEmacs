<xsl:stylesheet version="3.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xsl:mode on-no-match="shallow-copy"/>
  <xsl:output method="xml" indent="yes"/>
  <xsl:strip-space elements="*"/>
  <xsl:variable name="CJK-char" select="'[&#x3005;-&#x3007;&#x3021;-&#x3029;&#x3031;-&#x3035;&#x3038;-&#x303C;&#x3220;-&#x3229;&#x3248;-&#x324F;&#x3251;-&#x325F;&#x3280;-&#x3289;&#x32B1;-&#x32BF;&#x3400;-&#x4DB5;&#x4E00;-&#x9FEA;&#xF900;-&#xFA6D;&#xFA70;-&#xFAD9;]'"/>
  <xsl:variable name="line-break-regex">
    <xsl:text>(</xsl:text>
    <xsl:value-of select="$CJK-char"/>
    <xsl:text>)\s+(</xsl:text>
    <xsl:value-of select="$CJK-char"/>
    <xsl:text>)</xsl:text>
  </xsl:variable>
  <xsl:template match="//text()[ancestor-or-self::*[starts-with(@xml:lang,'zh')
                       or starts-with(@xml:lang,'ja')]]">
    <xsl:value-of select="replace(., $line-break-regex, '$1$2')"/>
  </xsl:template>
</xsl:stylesheet>
