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
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 化验单
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_test_report")
public class ErpTestReport extends Model<ErpTestReport> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 编号
     */
    private String code;
    /**
     * 产品id
     */
    private Integer product;
    /**
     * 产品批次id
     */
    private Integer batchId;
    /**
     * 产品批次名称
     */
    /*private String batchName;*/
    /**
     * 公司id
     */
    private Integer company;
    /**
     * 公司名称
     */
    private String companyName;
    /**
     * 化验类型id
     */
    private Integer testType;
    /**
     * 关联单号
     */
    private Integer orderNo;
    /**
     * 关联单号编号
     */
    private String orderCode;
    /**
     * 单据类别：采购，生产，销售，
     */
    private String orderType;
    /**
     * 客户id
     */
    private Integer customer;
    /**
     * 客户名称
     */
    private String customerName;
    /**
     * 炉号
     */
    private String boilerId;
    /**
     * 班次
     */
    private String shiftsId;
    /**
     * 化验人
     */
    private String testUser;
    /**
     * 化验日期
     */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date testDate;
    /**
     * 生产日期
     */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date productDate;
    /**
     * 创建人
     */
    private Integer createUser;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 化验结果，存储json
     */
    private String result;
    
    /**
     * 化验单状态,是否合格,从码表取值
     */
    private Integer state;
    
    /**
     * 逻辑删除字段，1删除，0未删除
     */
    private Integer deleted;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
