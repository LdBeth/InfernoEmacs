<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:html="http://www.w3.org/1999/xhtml"
                xmlns:lsml="https://ldbeth.sdf.org/lsml"
                xmlns="http://www.w3.org/1999/xhtml"
                exclude-result-prefixes="lsml html">

<xsl:output method="xml" encoding="utf-8" indent="no" html-version="5"
            omit-xml-declaration="yes"/>
<xsl:strip-space elements="*"/>
<xsl:preserve-space elements="html:pre lsml:code" />

<xsl:param name="preview-style"
           select="'display:none;font-size:1px;color:#ffffff;line-height:1px;max-height:0px;max-width:0px;opacity:0;overflow:hidden;'"/>
<xsl:param name="blockquote-style"
           select="'margin:0 0 0 .8ex;border-left:1px #ccc solid;padding-left:1ex'"/>
<xsl:template match="/">
  <html>
    <xsl:if test="/*/@lang">
      <xsl:attribute name="xml:lang">
        <xsl:value-of select="string(/*/@lang)"/>
      </xsl:attribute>
    </xsl:if>
    <head>
      <title><xsl:value-of select="/*/lsml:head/lsml:subject"/></title>
    </head>
    <body>
      <xsl:if test="/*/lsml:head/lsml:preview">
        <div style="$preview-style">
        <xsl:value-of select="/*/lsml:head/lsml:preview"/></div>
      </xsl:if>
      <div>
        <xsl:apply-templates select="/*/lsml:body" mode="body-text"/>
      </div>
    </body>
  </html>
</xsl:template>

<xsl:template match="lsml:p" mode="body-text">
  <p><xsl:apply-templates mode="body-text"/></p>
</xsl:template>

<xsl:template match="lsml:b" mode="body-text">
  <strong><xsl:apply-templates mode="body-text"/></strong>
</xsl:template>

<xsl:template match="lsml:i" mode="body-text">
  <em><xsl:apply-templates mode="body-text"/></em>
</xsl:template>

<xsl:template match="lsml:u" mode="body-text">
  <span style="text-decoration: underline;"><xsl:apply-templates mode="body-text"/></span>
</xsl:template>

<xsl:template match="lsml:ins" mode="body-text">
  <ins><xsl:apply-templates mode="body-text"/></ins>
</xsl:template>

<xsl:template match="lsml:del" mode="body-text">
  <del><xsl:apply-templates mode="body-text"/></del>
</xsl:template>

<xsl:template match="lsml:tt" mode="body-text">
  <code><xsl:apply-templates mode="body-text"/></code>
</xsl:template>

<xsl:template match="lsml:hr" mode="body-text">
  <hr/>
</xsl:template>

<xsl:template match="lsml:code" mode="body-text">
  <pre><xsl:apply-templates mode="body-text"/></pre>
</xsl:template>

<xsl:template match="lsml:l[@type='unord']" mode="body-text">
  <ul><xsl:apply-templates mode="body-text"/></ul>
</xsl:template>

<xsl:template match="lsml:l" mode="body-text">
  <ol><xsl:apply-templates mode="body-text"/></ol>
</xsl:template>

<xsl:template match="lsml:o" mode="body-text">
  <li><xsl:apply-templates mode="body-text"/></li>
</xsl:template>

<xsl:template match="lsml:quote" mode="body-text">
  <blockquote style="$blockquote-style">
    <xsl:apply-templates mode="body-text"/>
  </blockquote>
</xsl:template>

<xsl:template match="lsml:quote[@from]" mode="body-text">
  <blockquote style="$blockquote-style">
    <p class="attribution">
      <xsl:value-of select="string(@from)"/>
      <xsl:text> wrote:</xsl:text>
    </p>
  <xsl:apply-templates mode="body-text"/></blockquote>
</xsl:template>

<xsl:template match="lsml:quote[@from and @date]" mode="body-text">
  <blockquote style="$blockquote-style">
    <p class="attribution">
      <xsl:text>On </xsl:text>
      <xsl:value-of select="string(@date)"/>
      <xsl:text>, </xsl:text>
      <xsl:value-of select="string(@from)"/>
      <xsl:text> wrote:</xsl:text>
    </p>
  <xsl:apply-templates mode="body-text"/></blockquote>
</xsl:template>

<xsl:template match="lsml:verbatim" mode="body-text">
  <pre><xsl:value-of select="text()"/></pre>
</xsl:template>

<xsl:template match="lsml:section" mode="body-text">
  <xsl:variable name="seclevel" select="3+count(ancestor::lsml:section)"/>
  <xsl:variable name="normlevel">
    <xsl:choose>
      <xsl:when test="$seclevel > 6">
        <xsl:value-of select="6"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$seclevel"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name="label" select="concat('h',$normlevel)"/>
  <xsl:element name="{$label}">
    <xsl:value-of select="lsml:title"/>
  </xsl:element>
  <xsl:apply-templates mode="section-content"/>
</xsl:template>

<xsl:template match="lsml:title" mode="section-content"/>

<xsl:template match="*" mode="section-content">
  <xsl:apply-templates select="." mode="body-text"/>
</xsl:template>

<xsl:template match="lsml:raw" mode="body-text">
  <xsl:apply-templates select="html:*" mode="copy"/>
</xsl:template>

<xsl:template match="/ | @* | node()" mode="copy">
  <xsl:copy>
    <xsl:apply-templates select="@* | node()" mode="copy"/>
  </xsl:copy>
</xsl:template>

</xsl:stylesheet>
