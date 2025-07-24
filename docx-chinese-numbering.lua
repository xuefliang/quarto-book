-- docx-chinese-numbering-enhanced.lua
-- 增强版中文标题编号过滤器

-- 配置选项
local config = {
  enable_toc_prefix = true,  -- 是否启用目录前缀
  max_chinese_number = 99,   -- 中文数字的最大范围
  reset_on_new_chapter = true  -- 新章节时是否重置编号
}

-- 扩展的中文数字转换
local chinese_digits = {"", "一", "二", "三", "四", "五", "六", "七", "八", "九"}
local chinese_tens = {"", "十", "二十", "三十", "四十", "五十", "六十", "七十", "八十", "九十"}

-- 章节计数器
local counters = {
  level1 = 0, level2 = 0, level3 = 0, level4 = 0, level5 = 0, level6 = 0
}

-- 高级中文数字转换
function advanced_number_to_chinese(num)
  if num == 0 then return "" end
  if num > config.max_chinese_number then
    return tostring(num)
  end

  if num <= 10 then
    return chinese_digits[num + 1]
  elseif num < 20 then
    return "十" .. chinese_digits[num - 9]
  elseif num < 100 then
    local tens = math.floor(num / 10)
    local ones = num % 10
    return chinese_tens[tens + 1] .. chinese_digits[ones + 1]
  else
    -- 处理百位数
    local hundreds = math.floor(num / 100)
    local remainder = num % 100
    local result = chinese_digits[hundreds + 1] .. "百"

    if remainder > 0 then
      if remainder < 10 then
        result = result .. "零" .. chinese_digits[remainder + 1]
      else
        result = result .. advanced_number_to_chinese(remainder)
      end
    end
    return result
  end
end

-- 主处理函数
function Header(elem)
  local level = elem.level
  local title_text = pandoc.utils.stringify(elem.content)

  -- 跳过空标题
  if title_text == "" then
    return elem
  end

  -- 处理不同级别的标题
  if level == 1 then
    -- 第一章、第二章
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
    elem.content = {pandoc.Str(prefix .. " " .. title_text)}

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

  return elem
end

-- 可选：处理有序列表的中文编号
function OrderedList(elem)
  -- 如果需要，也可以将有序列表转换为中文编号
  return elem
end
