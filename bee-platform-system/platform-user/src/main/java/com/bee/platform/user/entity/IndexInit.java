package com.bee.platform.user.entity;

import java.io.Serializable;
import java.math.BigDecimal;

import com.baomidou.mybatisplus.annotations.TableField;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;
import net.bytebuddy.implementation.bytecode.assign.primitive.PrimitiveTypeAwareAssigner;

/**
 * 页面零时数据
 * @author junyang.li
 * @since 2019-03-20
 */
@Getter
@Setter
@Accessors(chain = true)
@ToString
@NoArgsConstructor
public class IndexInit extends Model<IndexInit> {

    private static final long serialVersionUID = 1L;

    /**
     * 入驻商家
     */
    private Integer registerEnterprise;
    /**
     * 物流发布量
     */
    private Integer publicRequirement;
    /**
     * 成交金额
     */
    private BigDecimal supplyMoney;

    private Integer chainEnterpriseCnt;

    private Integer chainServiceCnt;
    /**
     * 物流合作企业
     */
    @TableField("logistical_enterprise_cnt")
    private Integer logisticalEnterpriseCnt;
    /**
     * 物流单量
     */
    private Integer logisticalServiceCnt;
    /**
     * 完成订单数量
     */
    @TableField("finished_order_cnt")
    private Integer orderFulfill;
    /**
     * 物流运量
     */
    @TableField("transport_volume")
    private BigDecimal number9;

    private Integer number10;
    
    /**
     * 认证供应商(家)
     */
    private Integer countCompany;
    /**
     * 最新采购询价(单)
     */
    private Integer countVaildInquiry;
    /**
     * 报价中的供应商(家)
     */
    private Integer countVaildQuoted;
    /**
     * 成交总额
     */
    private BigDecimal countMoney;
    

    @Override
    protected Serializable pkVal() {
        return null;
    }

}
