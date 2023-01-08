<xsl:stylesheet version="3.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:mode on-no-match="fail"/>
  <xsl:output method="text" encoding="utf-8" indent="no"/>
  <xsl:strip-space elements="*"/>
  <xsl:template match="/">
    <xsl:apply-templates select="TeXML" mode="tex"/>
  </xsl:template>

  <xsl:template match="TeXML" mode="tex">
    <xsl:apply-templates mode="tex"/>
  </xsl:template>

  <xsl:template match="math" mode="tex">
    <xsl:text>$</xsl:text>
    <xsl:apply-templates mode="math"/>
    <xsl:text>$</xsl:text>
  </xsl:template>

  <xsl:template match="dmath" mode="tex">
    <xsl:text>$$</xsl:text>
    <xsl:apply-templates mode="math"/>
    <xsl:text>$$</xsl:text>
  </xsl:template>
  
  <xsl:template match="cmd" mode="tex">
    <xsl:text>\</xsl:text><xsl:value-of select="@name"/>
    <xsl:if test="opt">
      <xsl:variable name="options">
        <xsl:apply-templates select="opt" mode="tex"/>
      </xsl:variable>
      <xsl:text>[</xsl:text>
      <xsl:value-of select="options" separator=","/>
      <xsl:text>]</xsl:text>
    </xsl:if>
    <xsl:if test="parm">
      <xsl:text>{</xsl:text>
      <xsl:apply-templates select="parm" mode="tex"/>
      <xsl:text>}</xsl:text>
    </xsl:if>
    <xsl:variable name="next" select="following::node()[1][self::text()]"/>
    <xsl:if test="$next and not(parm)">
      <xsl:choose>
        <xsl:when test="matches($next, '^\s')">
          <xsl:text>{}</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text> </xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
  </xsl:template>

  <xsl:template match="env" mode="tex">
    <xsl:variable name="name" select="@name"/>
    <xsl:variable name="begin" select="if(@begin) then @begin else 'begin'"/>
    <xsl:variable name="end" select="if(@end) then @end else 'end'"/>
    <xsl:text>\</xsl:text>
    <xsl:value-of select="$begin"/>
    <xsl:text>{</xsl:text><xsl:value-of select="$name"/>
    <xsl:text>}</xsl:text>
    <xsl:apply-templates mode="tex"/>
    <xsl:text>\</xsl:text>
    <xsl:value-of select="$end"/>
    <xsl:text>{</xsl:text><xsl:value-of select="$name"/>
    <xsl:text>}</xsl:text>
  </xsl:template>

  <xsl:template match="spec" mode="tex">
    <xsl:choose>
      <xsl:when test="@ch = 'esc'">
        <xsl:text>\</xsl:text>
      </xsl:when>
      <xsl:when test="@ch = 'bg'">
        <xsl:text>{</xsl:text>
      </xsl:when>
      <xsl:when test="@ch = 'eg'">
        <xsl:text>}</xsl:text>
      </xsl:when>
      <xsl:when test="@ch = 'mshift'">
        <xsl:text>$</xsl:text>
      </xsl:when>
      <xsl:when test="@ch = 'align'">
        <xsl:text>&amp;</xsl:text>
      </xsl:when>
      <xsl:when test="@ch = 'parm'">
        <xsl:text>#</xsl:text>
      </xsl:when>
      <xsl:when test="@ch = 'sup'">
        <xsl:text>^</xsl:text>
      </xsl:when>
      <xsl:when test="@ch = 'sub'">
        <xsl:text>_</xsl:text>
      </xsl:when>
      <xsl:when test="@ch = 'tilde'">
        <xsl:text>~</xsl:text>
      </xsl:when>
      <xsl:when test="@ch = 'comment'">
        <xsl:text>%</xsl:text>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="text()" mode="tex">
    <xsl:value-of disable-output-escaping="yes"
                  select=". =>
                          replace('\$','\\\${}') =>
                          replace('\\','\$\\backslash\$') =>
                          replace('\{','\\{') =>
                          replace('\}','\\}') =>
                          replace('%','\\%{}') =>
                          replace('&amp;','\\&amp;{}') =>
                          replace('\|','\$|\$') =>
                          replace('#','\\#{}') =>
                          replace('_','\\_{}') =>
                          replace('\^','\\char`\\^{}') =>
                          replace('~','\\char`\\~{}') =>
                          replace('&lt;','\$&lt;\$') =>
                          replace('&gt;','\$&gt;\$')"/>
  </xsl:template>

  <xsl:template match="*" mode="math">
    <xsl:apply-templates select="." mode="tex"/>
  </xsl:template>
  <xsl:template match="text()" mode="math">
    <xsl:value-of disable-output-escaping="yes"
                  select=". =>
                          replace('\$','\\\${}') =>
                          replace('\\','\\backslash') =>
                          replace('\{','\\{') =>
                          replace('\}','\\}') =>
                          replace('%','\\%{}') =>
                          replace('&amp;','\\&amp;{}') =>
                          replace('#','\\#{}') =>
                          replace('_','\\_{}') =>
                          replace('\^','\\char`\\^{}') =>
                          replace('~','\\char`\\~{}')"/>
  </xsl:template>

</xsl:stylesheet>
