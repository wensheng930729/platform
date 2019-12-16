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
 * 物流付款
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-07-24
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_logistics_payment")
public class ErpLogisticsPayment extends Model<ErpLogisticsPayment> {

    private static final long serialVersionUID = 1L;

    /**
     * 物流付款id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 付款单号
     */
    private String payOrderNo;
    /**
     * 付款时间
     */
    private String payTime;
    /**
     * 物流订单id
     */
    private Integer orderId;
    /**
     * 物流订单号
     */
    private String orderNo;
    /**
     * 公司id
     */
    private Integer companyId;
    /**
     * 公司名称
     */
    private String companyName;
    /**
     * 承运商id
     */
    private Integer carrierId;
    /**
     * 承运商名称
     */
    private String carrierName;
    /**
     * 付款金额
     */
    private BigDecimal paymentAmount;
    /**
     * 支付类型,从码表取值
     */
    private String payType;
    /**
     * 备注
     */
    private String remarks;
    /**
     * 创建人id
     */
    private Integer createUser;
    /**
     * 跟新人id
     */
    private Integer updateUser;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 跟新时间
     */
    private Date updateTime;
    
    /**
     * 删除 0是未删除 1是删除
     */
    private Integer deleted;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
