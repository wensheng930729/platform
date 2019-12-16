package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.enums.IdType;

import io.swagger.annotations.ApiModelProperty;
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
 * 物流发票明细
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-07-24
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_logistics_invoice_detail")
public class ErpLogisticsInvoiceDetail extends Model<ErpLogisticsInvoiceDetail> {

    private static final long serialVersionUID = 1L;

    /**
     * 发票明细id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 物流订单id
     */
    private Integer orderId;
    /**
     * 物流发票id
     */
    private Integer invoiceId;
    /**
     * 产品id
     */
    private Integer productId;
    
    /*
     * 产品批次id
     */
	private Integer batchId;
    /**
     * 产品批次名称
     */
    private String batchName;
    /**
     * 产品名称
     */
    private String productName;
    /**
     * 产品单位
     */
    private String unit;
    /**
     * 重量
     */
    private BigDecimal weight;
    /**
     * 单价
     */
    private BigDecimal unitPrice;
    /**
     * 税率
     */
    private BigDecimal taxRate;
    /**
     * 总金额
     */
    private BigDecimal amount;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 修改时间
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
    
    /**
     * 删除：0是未删除，1是删除
     */
    private Integer deleted;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
