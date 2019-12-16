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
 * 销售单明细
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_sale_order_detail")
public class ErpSaleOrderDetail extends Model<ErpSaleOrderDetail> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 关联的销售单id
     */
    private Integer orderId;
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
     * 税额
     */
    private BigDecimal taxAmount;
    /**
     * 含税金额
     */
    private BigDecimal amount;
    
    /**
     * 是否删除
     */
    private Integer deleted;
    /**
     * 创建人id
     */
    private Integer createUser;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 产品批次id
     */
    private Integer productBatchId;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
