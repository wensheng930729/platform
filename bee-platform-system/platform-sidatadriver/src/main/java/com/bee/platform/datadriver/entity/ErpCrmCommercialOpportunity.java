package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * <p>
 * 商机信息
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-24
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@TableName("erp_crm_commercial_opportunity")
public class ErpCrmCommercialOpportunity extends Model<ErpCrmCommercialOpportunity> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 公司id
     */
    private Integer companyId;
    /**
     * 公司名字
     */
    private String companyName;
    /**
     * 客户id
     */
    private Integer customerId;
    /**
     * 客户名称
     */
    private String customerName;
    /**
     * 客户类型（码表取值）
     */
    private String customerType;
    /**
     * 所属行业
     */
    private String industry;
    /**
     * 获客渠道（码表取值）
     */
    private String customerObtainMethod;
    /**
     * 联系人id
     */
    private Integer contactId;
    /**
     * 联系人
     */
    private String contactName;
    /**
     * 联系方式
     */
    private String contactPhone;
    /**
     * 重要程度
     */
    private Integer degree;
    /**
     * 成交率
     */
    private Integer turnoverRatio;
    /**
     * 当前阶段（码表取值）
     */
    private String phase;
    /**
     * 销售员id
     */
    private Integer saleUserId;
    /**
     * 销售员名称
     */
    private String saleUserName;
    /**
     * 客户价值
     */
    private String customerValue;
    /**
     * 资金需求
     */
    private String fundRequirement;
    /**
     * 经营需求
     */
    private String manageRequirement;
    /**
     * 业务拓展
     */
    private String businessDevelopment;
    /**
     * 行业拓展
     */
    private String industryDevelopment;
    /**
     * 资源整合
     */
    private String resourcesIntegration;
    /**
     * 风险喜好（码表取值）
     */
    private String riskPreferences;
    /**
     * 逻辑删除字段，1删除，0未删除
     */
    private Integer deleted;
    /**
     * 创建人
     */
    private Integer createUser;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 修改人
     */
    private Integer updateUser;
    /**
     * 修改时间
     */
    private Date updateTime;
    /**
     * 备注
     */
    private String remark;
    /**
     * 产品营销备注
     */
    private String productRemark;
    /**
     * 详细街道地址
     */
    private String street;
    /**
     * 县级地区id
     */
    private Integer regionid;
    /**
     * 地址
     */
    private String address;
    
    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}