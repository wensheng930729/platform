package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.enums.IdType;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import java.io.Serializable;

/**
 * <p>
 * 物流发票
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-07-24
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_logistics_invoice")
public class ErpLogisticsInvoice extends Model<ErpLogisticsInvoice> {

    private static final long serialVersionUID = 1L;

    /**
     * 物流发票id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 物流订单id
     */
    private Integer orderId;
    /**
     * 发票单号
     */
    private String invoiceNumber;
    /**
     * 物流订单号
     */
    private String orderNumber;
    /**
     * 公司id
     */
    private Integer companyId;
    /**
     * 公司名称
     */
    private String companyName;
    /**
     * 开发票时间
     */
    private Date invoiceTime;
    /**
     * 承运商id
     */
    private Integer carrierId;
    /**
     * 承运商名称
     */
    private String carrierName;
    /**
     * 备注
     */
    private String remarks;
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



    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
