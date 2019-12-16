package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
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
 * 采购发票
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-01
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_purchase_invoice_order")
public class ErpPurchaseInvoiceOrder extends Model<ErpPurchaseInvoiceOrder> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 编号
     */
    private String code;
    /**
     * 采购单id
     */
    private Integer purchaseOrder;
    /**
     * 采购单编号
     */
    private String purchaseOrderNo;
    /**
     * 公司
     */
    private Integer company;
    /**
     * 公司名称
     */
    private String companyName;
    /**
     * 开票日期
     */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date invoiceDate;
    /**
     * 供应商id
     */
    private Integer supplier;
    /**
     * 供应商名称
     */
    private String supplyName;
    /**
     * 逻辑删除字段，1删除，0未删除
     */
    private Integer deleted;
    /**
     * 备注
     */
    private String remark;
    /**
     * 确认状态(0已保存，1已确认)
     */
    private Integer state;
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
