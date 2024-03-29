<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:html="http://www.w3.org/1999/xhtml"
                xmlns:lsml="https://ldbeth.sdf.org/lsml"
                xmlns="http://www.w3.org/1999/xhtml"
                exclude-result-prefixes="lsml html">

<xsl:output method="xml" encoding="utf-8" indent="no" html-version="5"
            omit-xml-declaration="yes"/>
<xsl:param name="preview-style"
           select="'display:none;font-size:1px;color:#ffffff;line-height:1px;max-height:0px;max-width:0px;opacity:0;overflow:hidden;'"/>
<xsl:param name="blockquote-style"
           select="'margin:0 0 0 .8ex;border-left:4px #ccc solid;padding-left:1ex;color:#333;'"/>

<xsl:param name="attribution-style"
           select="'font-family:monospace;'"/>

<xsl:param name="kbd-style"
           select="'font-family:monospace;border-radius:.25rem;background:#000033;color:#ddd;'"/>

<xsl:template match="/">
  <xsl:text disable-output-escaping="yes">&lt;!DOCTYPE html&gt;</xsl:text>
  <html>
    <xsl:if test="/*/@lang">
      <xsl:attribute name="lang">
        <xsl:value-of select="string(/*/@lang)"/>
      </xsl:attribute>
    </xsl:if>
    <head>
      <xsl:if test="/*/lsml:head/lsml:Subject != ''">
        <title><xsl:value-of select="/*/lsml:head/lsml:Subject"/></title>
      </xsl:if>
      <meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
    </head>
    <body style="margin:0;padding:0;">
      <xsl:if test="/*/lsml:head/lsml:preview">
        <div style="{$preview-style}">
        <xsl:value-of select="/*/lsml:head/lsml:preview"/></div>
      </xsl:if>
      <div style="margin:70px;font-family: Arial, Helvetica, sans-serif;">
        <xsl:apply-templates select="/*/lsml:body" mode="body-text"/>
        <xsl:if test="/*/lsml:sig">
          <hr/>
          <div class="signature">
            <xsl:apply-templates select="/*/lsml:sig" mode="body-text"/>
          </div>
        </xsl:if>
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

<xsl:template match="lsml:br" mode="body-text">
  <br/>
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

<xsl:template match="lsml:c" mode="body-text">
  <cite><xsl:apply-templates mode="body-text"/></cite>
</xsl:template>

<xsl:template match="lsml:url" mode="body-text">
  <a href="{text()}"><xsl:value-of select="text()"/></a>
</xsl:template>

<xsl:template match="lsml:img" mode="body-text">
  <img><xsl:copy-of select="@*"/></img>
</xsl:template>

<xsl:template match="lsml:kbd" mode="body-text">
  <kbd style="{$kbd-style}"><xsl:value-of select="text()"/></kbd>
</xsl:template>

<xsl:template match="lsml:quote" mode="body-text">
  <blockquote style="{$blockquote-style}">
    <xsl:if test="@from or @date or @cite">
      <p>
        <xsl:if test="@date">
           <xsl:text>On </xsl:text>
           <xsl:value-of select="string(@date)"/>
           <xsl:text>, </xsl:text>
        </xsl:if>
        <xsl:if test="@from">
          <xsl:value-of select="string(@from)"/>
          <xsl:text> wrote:</xsl:text>
        </xsl:if>
         <xsl:if test="@cite">
           <xsl:text> (</xsl:text><a
           href="{@cite}">original message</a><xsl:text>)</xsl:text>
        </xsl:if>
      </p>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="html:*">
        <xsl:apply-templates select="html:*" mode="copy"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates mode="body-text"/>
      </xsl:otherwise>
    </xsl:choose>
  </blockquote>
</xsl:template>

<xsl:template match="lsml:vquote" mode="body-text">
  <blockquote style="{$blockquote-style}">
    <pre><xsl:value-of select="text()"/></pre>
  </blockquote>
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
    <xsl:attribute name="style">
      <xsl:value-of select="'color:#660000;'"/>
    </xsl:attribute>
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

<xsl:template match="@* | node()" mode="copy">
  <xsl:copy>
    <xsl:apply-templates select="@* | node()" mode="copy"/>
  </xsl:copy>
</xsl:template>

</xsl:stylesheet>
