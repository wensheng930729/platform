package com.bee.platform.datadriver.enums;

import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @Classname EnumPriceType
 * @Description 价格计算参数
 * @Date 2019/6/3 13:57
 * @Author xin.huang
 */
@Getter
@NoArgsConstructor
public enum EnumPriceType {
    /**
     * 销售价格计算参数
     */
    SALE_STATEMENT(50);

    private Integer key;

    EnumPriceType(Integer key){
        this.key=key;
    }
}
