package com.bee.platform.user.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @description:企业转换类
 * @author: junyang.li
 * @create: 2018-11-29 15:11
 **/
@JsonInclude(JsonInclude.Include.NON_NULL)
@Getter
@Setter
@Accessors(chain = true)
public class CompanyDTO implements Serializable {

    private static final long serialVersionUID = 2414012561332275949L;
    /**
     * 企业id
     */
    private Integer companyId;
    /**
     * 企业名称
     */
    private String company;
    /**
     * 企业logo
     */
    private String logoUrl;
    /**
     * 主营行业关联id，与t_industry_unit表关联
     */
    private Integer industryUnitId;
    /**
     * 主营行业名称
     */
    private String industryUnitName;
    /**
     * 经营模式
     */
    private String operationMode;
    /**
     * 公司所在地
     */
    private String address;
    /**
     * 固定电话
     */
    private String fixTel;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 修改时间
     */
    private Date updateTime;

    //企业类型 1企业 2物流商
    private int type;
    //企业所属行业
    private String industry;

    public CompanyDTO() {

    }

    public CompanyDTO(EnterpriseDetailDTO enterprise) {
        this.companyId = enterprise.getId();
        this.company = enterprise.getName();
        this.logoUrl = enterprise.getHead();
        //this.industryUnitId = industryUnitId;
        //this.industryUnitName = industryUnitName;
        //this.operationMode = operationMode;
        this.address = enterprise.getAddress();
        this.fixTel = enterprise.getContact();
        this.type=enterprise.getType();
    }

}
