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
import com.baomidou.mybatisplus.activerecord.Model;

/**
 * <p>
 * 物流订单明细
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-07-24
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_logistics_orders_detail")
public class ErpLogisticsOrdersDetail extends Model<ErpLogisticsOrdersDetail> {

    private static final long serialVersionUID = 1L;

    /**
     * 物流订单明细id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 物流订单id
     */
    private Integer orderId;
    /**
     * 商品id
     */
    private Integer productId;
    /**
     * 产品名称
     */
    private String productName;
    /**
     * 批次id
     */
    private Integer batchId;
    
    /**
     * 批次名称
     */
    private String batchName;
    
    /**
     * 产品单位
     */
    private String unit;
    /**
     * 数量
     */
    private BigDecimal number;
    /**
     * 含税运费单价
     */
    private BigDecimal unitPrice;
    /**
     * 税率
     */
    private BigDecimal taxRate;
    /**
     * 无税金额
     */
    private BigDecimal freeTaxAmount;
    /**
     * 税额
     */
    private BigDecimal tax;
    /**
     * 含税金额
     */
    private BigDecimal taxAmount;
    /**
     * 创建人id
     */
    private Integer createUser;
    /**
     * 修改人id
     */
    private Integer updateUser;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 修改时间
     */
    private Date updateTime;
    /**
     * 删除：0是未删除，1是删除
     */
    private Integer deleted;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
