package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.enums.IdType;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import java.io.Serializable;

/**
 * <p>
 * 物流结算
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-07-24
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_logistics_settlement")
public class ErpLogisticsSettlement extends Model<ErpLogisticsSettlement> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 物流订单id
     */
    private Integer orderId;
    /**
     * 结算单号
     */
    private String statementNo;
    /**
     * 结算日期
     */
    private Date statementTime;
    /**
     * 合同重量
     */
    private BigDecimal contractWeight;
    /**
     * 收货重量
     */
    private BigDecimal receivedWeight;
    /**
     * 结算重量
     */
    private BigDecimal settlementWeight;
    /**
     * 合同单价
     */
    private BigDecimal contractPrice;
    /**
     * 结算单价
     */
    private BigDecimal settlementPrice;
    /**
     * 结算扣款
     */
    private BigDecimal settlementDeductions;
    /**
     * 结算总额
     */
    private BigDecimal settlementAmount;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 跟新时间
     */
    private Date updateTime;
    /**
     * 创建人id
     */
    private Integer createUser;
    /**
     * 修改人id
     */
    private Integer updateUser;



    @Override
    protected Serializable pkVal() {
        return this.id;
    }
}
