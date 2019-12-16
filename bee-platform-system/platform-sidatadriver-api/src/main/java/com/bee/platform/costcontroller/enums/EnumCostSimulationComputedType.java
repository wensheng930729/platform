package com.bee.platform.costcontroller.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @ClassName EnumAccountStatus
 * @Description 结算类型
 * @author jie.chen
 * @Date 2019/5/29$ 17:44$
 * @version 1.0.0
 */

@Getter
@AllArgsConstructor
@NoArgsConstructor
public enum EnumCostSimulationComputedType {

    /**
     * 计算结果和指标录入类型
     */
    SELF(1,"未结算"),
    FAST(2,"快检"),
    COMMODITY(3,"商检");

    private Integer key;
    private String value;
}
