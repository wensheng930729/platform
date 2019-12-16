package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import java.math.BigDecimal;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 销售收款单主表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_receipt_order")
public class ErpReceiptOrder extends Model<ErpReceiptOrder> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 收款单编号
     */
    private String code;
    /**
     * 收款公司id
     */
    private Integer companyId;
    /**
     * 收款公司名称
     */
    private String companyName;
    /**
     * 收款日期
     */
    private Date receiptTime;
    /**
     * 客户公司id
     */
    private Integer customerId;
    /**
     * 客户公司名称
     */
    private String customerName;
    /**
     * 支付方式，从码表取值
     */
    private String payMethod;
    /**
     * 备注
     */
    private String remark;
    /**
     * 收款金额
     */
    private BigDecimal amount;
    /**
     * 收款状态（未收款，已收款）
     */
    private Integer state;
    /**
     * 创建人id
     */
    private Integer creatorId;
    /**
     * 创建人企业id
     */
    private Integer creatorEnterpriseId;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 附件名
     */
    private String fileName;
    /**
     * 附件url
     */
    private String fileUrl;
    /**
     * 逻辑删除字段，1删除，0未删除
     */
    private Integer deleted;
    /**
     * 修改人id
     */
    private Integer modifierId;
    /**
     * 修改时间
     */
    private Date modifyTime;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }


}
