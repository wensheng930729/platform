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
import io.swagger.annotations.ApiModel;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 付款单
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("付款单列表信息")
@TableName("erp_pay_order")
public class ErpPayOrder extends Model<ErpPayOrder> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 编号
     */
    private String code;
    /**
     * 公司
     */
    private Integer company;
    /**
     * 公司名称
     */
    private String companyName;
    /**
     * 金额
     */
    private BigDecimal amount;
    /**
     * 付款日期
     */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date payDate;
    /**
     * 采购单id
     */
    private Integer purchaseOrder;
    /**
     * 采购单号
     */
    private String purchaseOrderNo;
    /**
     * 供应商id
     */
    private Integer supplier;
    /**
     * 供应商名称
     */
    private String supplyName;
    /**
     * 附件地址
     */
    private String url;
    /**
     * 备注
     */
    private String remark;
    /**
     * 支付方式，从码表取值
     */
    private String payMethod;
    /**
     * 确认状态(0已保存，1已确认)
     */
    private Integer state;
    /**
     * 确认状态(0否，1是)
     */
    private Integer deleted;
    /**
     * 创建人
     */
    private Integer createUser;
    /**
     * 修改人
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

    @Override
    protected Serializable pkVal() {
        return this.id;
    }
}
