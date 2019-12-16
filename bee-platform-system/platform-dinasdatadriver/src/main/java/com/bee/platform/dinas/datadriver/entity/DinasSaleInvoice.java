package com.bee.platform.dinas.datadriver.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.enums.IdType;
import java.math.BigDecimal;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Data;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 销售发票
 * </p>
 *
 * @author huangxin123
 * @since 2019-08-13
 */
@Data
@ToString
@Accessors(chain = true)
public class DinasSaleInvoice extends Model<DinasSaleInvoice> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 合同id
     */
    private Integer orderId;
    /**
     * 订货商id
     */
    private Integer customerId;
    /**
     * 产品id
     */
    private Integer productId;
    /**
     * 公司id
     */
    private Integer companyId;
    /**
     * 回款单号
     */
    private String companyName;
    /**
     * 产品规格id
     */
    private Integer productSpecId;
    /**
     * 开票数量
     */
    private BigDecimal invoiceNum;
    /**
     * 开票金额
     */
    private BigDecimal invoiceAmount;
    /**
     * 开票日期
     */
    private Date invoiceDate;
    /**
     * 发票附件
     */
    private String url;
    /**
     * 创建人id
     */
    private Integer createUser;
    /**
     * 更新人id
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
     * 删除
     */
    private Integer deleted;
    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
