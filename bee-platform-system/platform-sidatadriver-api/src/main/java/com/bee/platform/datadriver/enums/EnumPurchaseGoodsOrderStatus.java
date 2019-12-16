package com.bee.platform.datadriver.enums;

import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @Classname EnumPurchaseGoodsOrderStatus
 * @Description 采购收货订单状态
 * @Date 2019/5/30 10:14
 * @Author xin.huang
 */
@Getter
@NoArgsConstructor
public enum EnumPurchaseGoodsOrderStatus {

    /**
     * 待确认
     */
    SAVED(0),
    /**
     * 已确认
     */
    CONFIRMED(1);

    private Integer key;

    EnumPurchaseGoodsOrderStatus(Integer key){
        this.key=key;
    }
}
