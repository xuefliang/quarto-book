-- docx-chinese-numbering-enhanced.lua
-- 增强版中文标题编号过滤器（所有一级标题都分页且在DOCX中居中）

-- 配置选项
local config = {
  enable_toc_prefix = true,  -- 是否启用目录前缀
  max_chinese_number = 99,   -- 中文数字的最大范围
  reset_on_new_chapter = true,  -- 新章节时是否重置编号
  auto_page_break = true     -- 是否在新章节前自动分页
}

-- 扩展的中文数字转换
local chinese_digits = {"", "一", "二", "三", "四", "五", "六", "七", "八", "九"}
local chinese_tens = {"", "十", "二十", "三十", "四十", "五十", "六十", "七十", "八十", "九十"}

-- 章节计数器
local counters = {
  level1 = 0, level2 = 0, level3 = 0, level4 = 0, level5 = 0, level6 = 0
}

-- 不需要章节编号的特殊标题（前言、参考文献、附录等）
local special_titles = {
  -- 中文
  ["前言"] = true,
  ["序言"] = true,
  ["绪论"] = true,
  ["摘要"] = true,
  ["目录"] = true,
  ["参考文献"] = true,
  ["参考资料"] = true,
  ["致谢"] = true,
  ["谢辞"] = true,
  ["附录"] = true,
  ["附件"] = true,
  ["结论"] = true,
  ["总结"] = true,
  ["结语"] = true,
  ["后记"] = true,
  -- 英文
  ["abstract"] = true,
  ["preface"] = true,
  ["introduction"] = true,
  ["conclusion"] = true,
  ["references"] = true,
  ["bibliography"] = true,
  ["acknowledgments"] = true,
  ["acknowledgements"] = true,
  ["appendix"] = true,
  ["appendices"] = true,
  ["contents"] = true,
  ["summary"] = true
}

-- 检查是否为特殊标题
function is_special_title(title)
  local lower_title = string.lower(title)

  -- 直接匹配
  if special_titles[title] or special_titles[lower_title] then
    return true
  end

  -- 模糊匹配
  for pattern, _ in pairs(special_titles) do
    if string.find(lower_title, pattern, 1, true) then
      return true
    end
  end

  -- 附录的特殊匹配（附录A、附录1等）
  if string.match(title, "^附录[A-Z0-9一二三四五六七八九十]") then
    return true
  end

  if string.match(lower_title, "^appendix%s*[a-z0-9]") then
    return true
  end

  return false
end

-- 高级中文数字转换
function advanced_number_to_chinese(num)
    if not num or num == 0 then return "" end  -- 添加 not num 检查
    if num > config.max_chinese_number then
        return tostring(num)
    end

    if num <= 10 then
        if num + 1 <= #chinese_digits then
            return chinese_digits[num + 1] or ""
        else
            return tostring(num)
        end
    elseif num < 20 then
        local digit = chinese_digits[num - 9] or ""
        return "十" .. digit
    elseif num < 100 then
        local tens = math.floor(num / 10)
        local ones = num % 10
        local tens_str = chinese_tens[tens + 1] or ""
        local ones_str = chinese_digits[ones + 1] or ""
        return tens_str .. ones_str
    else
        -- 处理百位数
        local hundreds = math.floor(num / 100)
        local remainder = num % 100
        local hundreds_str = chinese_digits[hundreds + 1] or ""
        local result = hundreds_str .. "百"

        if remainder > 0 then
            if remainder < 10 then
                local remainder_str = chinese_digits[remainder + 1] or ""
                result = result .. "零" .. remainder_str
            else
                local remainder_converted = advanced_number_to_chinese(remainder)
                result = result .. (remainder_converted or "")
            end
        end
        return result
    end
end


-- 创建分页符
function create_page_break()
  if FORMAT:match 'docx' then
    return pandoc.RawBlock('openxml', '<w:p><w:r><w:br w:type="page"/></w:r></w:p>')
  else
    return pandoc.RawBlock('html', '<div style="page-break-before: always;"></div>')
  end
end

-- 创建居中的DOCX标题
function create_centered_docx_header(title_text)
  local openxml_content = string.format([[
<w:p>
  <w:pPr>
    <w:jc w:val="center"/>
    <w:pStyle w:val="Heading1"/>
  </w:pPr>
  <w:r>
    <w:t>%s</w:t>
  </w:r>
</w:p>
]], title_text)

  return pandoc.RawBlock('openxml', openxml_content)
end

-- 清理自动编号的函数
function clean_auto_numbering(text)
  text = text:gsub("^%d+%.%s*", "")           -- 移除 "1. "
  text = text:gsub("^%d+%.%d+%.?%s*", "")     -- 移除 "1.1 " 或 "1.1."
  text = text:gsub("^%d+%.%d+%.%d+%.?%s*", "") -- 移除 "1.1.1 " 或 "1.1.1."
  text = text:gsub("^%d+%.%d+%.%d+%.%d+%.?%s*", "") -- 移除 "1.1.1.1 " 或 "1.1.1.1."
  return text
end

-- 主处理函数
function Header(elem)
  local level = elem.level
  local title_text = pandoc.utils.stringify(elem.content)

  -- 清理可能存在的自动编号
  title_text = clean_auto_numbering(title_text)

  -- 跳过空标题
  if title_text == "" then
    return elem
  end

  local result = {}

  -- 处理不同级别的标题
  if level == 1 then
    -- 所有一级标题都需要分页
    if config.auto_page_break then
      table.insert(result, create_page_break())
    end

    local final_title_text
    -- 检查是否为特殊标题
    if is_special_title(title_text) then
      -- 特殊标题不编号，保持原标题
      final_title_text = title_text
    else
      -- 普通章节标题，添加章节编号
      counters.level1 = counters.level1 + 1
      if config.reset_on_new_chapter then
        counters.level2 = 0
        counters.level3 = 0
        counters.level4 = 0
        counters.level5 = 0
        counters.level6 = 0
      end

      local chapter_num = advanced_number_to_chinese(counters.level1)
      local prefix = "第" .. chapter_num .. "章"
      final_title_text = prefix .. " " .. title_text
    end

    -- 为DOCX格式创建居中的标题，其他格式使用普通标题
    if FORMAT:match 'docx' then
      table.insert(result, create_centered_docx_header(final_title_text))
    else
      elem.content = {pandoc.Str(final_title_text)}
      elem.attributes = elem.attributes or {}
      elem.attributes["number"] = "false"
      table.insert(result, elem)
    end

  elseif level == 2 then
    -- 第一节、第二节
    counters.level2 = counters.level2 + 1
    counters.level3 = 0
    counters.level4 = 0
    counters.level5 = 0
    counters.level6 = 0

    local section_num = advanced_number_to_chinese(counters.level2)
    local prefix = "第" .. section_num .. "节"
    elem.content = {pandoc.Str(prefix .. " " .. title_text)}

  elseif level == 3 then
    -- 一、二、三
    counters.level3 = counters.level3 + 1
    counters.level4 = 0
    counters.level5 = 0
    counters.level6 = 0

    local subsection_num = advanced_number_to_chinese(counters.level3)
    elem.content = {pandoc.Str(subsection_num .. "、" .. title_text)}

  elseif level == 4 then
    -- 1、2、3
    counters.level4 = counters.level4 + 1
    counters.level5 = 0
    counters.level6 = 0

    elem.content = {pandoc.Str(tostring(counters.level4) .. "、" .. title_text)}

  elseif level == 5 then
    -- (1)、(2)、(3)
    counters.level5 = counters.level5 + 1
    counters.level6 = 0

    elem.content = {pandoc.Str("(" .. tostring(counters.level5) .. ") " .. title_text)}

  elseif level == 6 then
    -- ①、②、③
    counters.level6 = counters.level6 + 1

    local circle_numbers = {"①", "②", "③", "④", "⑤", "⑥", "⑦", "⑧", "⑨", "⑩"}
    local prefix = counters.level6 <= 10 and circle_numbers[counters.level6] or ("(" .. counters.level6 .. ")")
    elem.content = {pandoc.Str(prefix .. " " .. title_text)}
  end

  -- 对于非一级标题或非DOCX格式，禁用自动编号
  if level ~= 1 or not FORMAT:match 'docx' then
    elem.attributes = elem.attributes or {}
    elem.attributes["number"] = "false"
    table.insert(result, elem)
  end

  -- 返回结果数组或单个元素
  return #result > 1 and result or (result[1] or elem)
end

-- 禁用自动编号
function Meta(meta)
  meta["number-sections"] = false
  return meta
end

-- 文档级别处理
function Pandoc(doc)
  doc.meta = doc.meta or {}
  doc.meta["number-sections"] = false
  return doc
end
