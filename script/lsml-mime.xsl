<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:lsml="https://ldbeth.sdf.org/lsml"
                xmlns:html="http://www.w3.org/1999/xhtml"
                xmlns="https://ldbeth.sdf.org/lsml"
                exclude-result-prefixes="lsml html">

<xsl:output method="xml" encoding="utf-8" indent="no"
            omit-xml-declaration="yes"/>
<xsl:strip-space elements="*"/>
<xsl:preserve-space elements="html:pre lsml:code lsml:vquote"/>

<xsl:template match="/lsml:lsml">
  <xsl:copy>
    <xsl:copy-of select="@*"/>
    <config>
      <xsl:if test="/*/@format='usetext'">
        <usetext/>
      </xsl:if>
      <headers>
        <xsl:attribute name="Subject">
          <xsl:value-of select="/*/lsml:head/lsml:Subject"/>
        </xsl:attribute>
        <xsl:for-each select="/*/lsml:head/lsml:From |
                              /*/lsml:head/lsml:To |
                              /*/lsml:head/lsml:Cc |
                              /*/lsml:head/lsml:Bcc">
          <xsl:attribute name="{local-name(.)}">
            <xsl:apply-templates select="." mode="email"/>
          </xsl:attribute>
        </xsl:for-each>
        <xsl:if test="/*/lsml:head/lsml:headers">
          <xsl:for-each select="/*/lsml:head/lsml:headers/*">
            <xsl:attribute name="{local-name(.)}">
              <xsl:value-of select="."/>
            </xsl:attribute>
          </xsl:for-each>
        </xsl:if>
      </headers>
      <mime>
        <xsl:apply-templates select="/*/lsml:body" mode="mime"/>
      </mime>
    </config>
    <xsl:apply-templates select="node()" mode="copy"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="*" mode="mime">
    <xsl:apply-templates mode="mime"/>
</xsl:template>

<xsl:template match="@*|text()" mode="mime"/>

<xsl:template match="lsml:img" mode="mime">
  <item>
    <xsl:attribute name="cid">
      <xsl:value-of select="generate-id(.)"/>
    </xsl:attribute>
    <xsl:attribute name="src">
      <xsl:value-of select="@src"/>
    </xsl:attribute>
  </item>
</xsl:template>

<xsl:template match="@* | node()" mode="copy">
  <xsl:copy>
    <xsl:apply-templates select="@* | node()" mode="copy"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="lsml:img" mode="copy">
  <img>
    <xsl:copy-of select="@*"/>
    <xsl:attribute name="src">
      <xsl:value-of select="concat('cid:',generate-id(.))"/>
    </xsl:attribute>
  </img>
</xsl:template>


<xsl:template match="*" mode="email">
  <xsl:for-each select="lsml:email">
    <xsl:if test="position() > 1">, </xsl:if>
    <xsl:choose>
      <xsl:when test="preceding-sibling::lsml:name[1] = preceding-sibling::*[1]">
        <xsl:value-of select="preceding-sibling::lsml:name[1]"/>
        <xsl:text> &lt;</xsl:text>
        <xsl:value-of select="."/>
        <xsl:text>&gt;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:for-each>
</xsl:template>

</xsl:stylesheet>
