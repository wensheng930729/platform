package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import java.math.BigDecimal;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 采购单明细
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_purchase_order_detail")
public class ErpPurchaseOrderDetail extends Model<ErpPurchaseOrderDetail> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 采购单id
     */
    private Integer orderId;
    /**
     * 产品
     */
    private Integer productId;
    /**
     * 产品数量
     */
    private BigDecimal num;
    /**
     * 单位
     */
    private String unit;
    /**
     * 含税单价
     */
    private BigDecimal taxPrice;
    /**
     * 税率，从码表取值
     */
    private String taxRate;
    /**
     * 无税金额
     */
    private BigDecimal taxFreeAmount;
    /**
     * 税额
     */
    private BigDecimal taxAmount;
    /**
     * 含税金额
     */
    private BigDecimal amount;
    /**
     * 逻辑删除字段，1删除，0未删除
     */
    private Integer deleted;
    /**
     * 创建人
     */
    private Integer createUser;
    /**
     * 创建时间
     */
    private Date createTime;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
