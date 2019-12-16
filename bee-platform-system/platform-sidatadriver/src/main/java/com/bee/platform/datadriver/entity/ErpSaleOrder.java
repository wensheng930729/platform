package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import java.math.BigDecimal;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 销售单
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_sale_order")
public class ErpSaleOrder extends Model<ErpSaleOrder> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 编号
     */
    private String contractNo;
    /**
     * 公司id
     */
    private Integer company;
    
    /**
     * 公司名称
     */
    private String companyName;
    /**
     * 签订日期
     */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date contractDate;
    /**
     * 客户id
     */
    private Integer customer;
    /**
     * 客户名称
     */
    private String customerName;
    /**
     * 备注
     */
    private String remark;
    /**
     * 销售方式(0包运，1自提)
     */
    private Integer sellMethod;
    /**
     * 数量
     */
    private BigDecimal totalNum;
    /**
     * 金额
     */
    private BigDecimal amount;
    /**
     * 发票状态，从码表取
     */
    private Integer invoceState;
    /**
     * 执行状态
     */
    private Integer state;
    /**
     * 发货状态
     */
    private Integer deliveryState;
    /**
     * 结算状态
     */
    private Integer accountState;
    /**
     * 炉号
     */
    private String boilerId;
    /**
     * 班次
     */
    private String shiftsId;
    /**
     * 创建人
     */
    private Integer createUser;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 
     */
    private Integer deleted;
    /**
     * 累计收款金额
     */
    private BigDecimal totalReceiptAmount;
    /**
     * 合同质量要求
     */
    private String ContractQualityRequirements;
    /**
     * 销售员id
     */
    private Integer saleUserId;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }
}
