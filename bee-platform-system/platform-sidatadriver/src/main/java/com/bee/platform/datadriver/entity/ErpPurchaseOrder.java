package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import java.math.BigDecimal;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 采购订单
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_purchase_order")
public class ErpPurchaseOrder extends Model<ErpPurchaseOrder> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 合同编号
     */
    private String contractNo;
    /**
     * 合同签订日期
     */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date contractTime;
    /**
     * 供应商id
     */
    private Integer supplier;
    /**
     * 供应商名称
     */
    private String supplyName;
    /**
     * 公司id
     */
    private Integer company;
    /**
     * 公司名称
     */
    private String companyName;
    /**
     * 采购地
     */
    private String place;
    /**
     * 采购方式(0 现货 1 期货)
     */
    private Integer purchaseMethod;
    /**
     * 合同质量要求
     */
    private String requirement;
    /**
     * 备注
     */
    private String remark;
    /**
     * 发票状态
     */
    private Integer invoiceState;
    /**
     * 收货状态
     */
    private Integer receiveState;
    /**
     * 执行状态
     */
    private Integer state;
    /**
     * 付款状态
     */
    private Integer payState;
    /**
     * 结算状态
     */
    private Integer accountState;
    /**
     * 总金额
     */
    private BigDecimal amount;
    /**
     * 总量
     */
    private BigDecimal totalNum;
    /**
     * 单位
     */
    private String unit;
    /**
     * 炉号
     */
    private String boilerId;
    /**
     * 班次
     */
    private String shiftsId;
    /**
     * 创建人
     */
    private Integer purchaseUserId;
    /**
     * 创建人
     */
    private Integer createUser;
    /**
     * 逻辑删除字段，1删除，0未删除
     */
    private Integer deleted;
    /**
     * 状态，从码表取值
     */
    private Date createTime;
    /**
     * 更新时间
     */
    private Date updateTime;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }
}
