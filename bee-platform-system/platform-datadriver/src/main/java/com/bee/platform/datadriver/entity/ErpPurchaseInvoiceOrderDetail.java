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
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 采购发票明细
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-01
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_purchase_invoice_order_detail")
public class ErpPurchaseInvoiceOrderDetail extends Model<ErpPurchaseInvoiceOrderDetail> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 发票id
     */
    private Integer orderId;
    /**
     * 产品id
     */
    private Integer productId;
    /**
     * 单价
     */
    private BigDecimal price;
    /**
     * 产品数量
     */
    private BigDecimal num;
    /**
     * 单位
     */
    private String unit;
    /**
     * 税率，从码表取值
     */
    private String taxRate;
    /**
     * 金额
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
     * 更新人
     */
    private Integer updateUser;
    /**
     * 创建时间
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
