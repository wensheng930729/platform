package com.bee.platform.datadriver.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@AllArgsConstructor
@NoArgsConstructor
public enum EnumErpCode {
	 /**
     * 储备客户
     */
	RESERVE_CUSTOMERS(0,"储备客户"),

    /**
     * 拜访客户
     */
	VISITING_CUSTOMERS(1,"拜访客户"),
	
	/**
	 * 推进客户
	 */
	PROMOTING_CUSTOMERS(2,"推进客户"),
	/**
	 * 成交客户
	 */
	TRANSACTION_CLIENTS(3,"成交客户");
	private Integer key;
    private String value;
}
