package com.bee.platform.user.utils;

import net.sourceforge.pinyin4j.PinyinHelper;
import net.sourceforge.pinyin4j.format.HanyuPinyinOutputFormat;
import net.sourceforge.pinyin4j.format.HanyuPinyinToneType;
import net.sourceforge.pinyin4j.format.HanyuPinyinVCharType;

/**
 * Created by CrazyMouse on 2016/11/3.
 */
public class PinyinUtil {

	private static HanyuPinyinOutputFormat PINYIN_FORMAT;
	
	static {
		PINYIN_FORMAT = new HanyuPinyinOutputFormat();
		PINYIN_FORMAT.setToneType(HanyuPinyinToneType.WITHOUT_TONE);
		PINYIN_FORMAT.setVCharType(HanyuPinyinVCharType.WITH_V);
	}
	
	public static String toPinyin(String input) {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < input.length(); i++) {
			char c = input.charAt(i);
			if (c <= 255) {
				sb.append(c);
			} else {
				String pinyin = null;
				try {
					String[] pinyinArray = PinyinHelper.toHanyuPinyinStringArray(c, PINYIN_FORMAT);
					pinyin = pinyinArray[0];
				} catch (Exception e) {
					return sb.toString();
				}
				if (pinyin != null) {
					sb.append(pinyin);
				}
			}
		}
		return sb.toString();
	}
}
