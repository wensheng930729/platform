package com.bee.platform.user.entity;

import com.baomidou.mybatisplus.annotations.TableName;
import lombok.Data;

import java.time.LocalDateTime;

/**
 * @ClassName: CustomerServiceInfo
 * @Description: java类作用描述
 * @Author: fei.sun
 * @Date: 2019/5/20 10:58
 * @Version: 1.0
 */
@Data
@TableName("m_baseinfo")
public class CustomerServiceInfo {

    private Integer id;
    private String customerServiceHotline;
    private String officePhone;
    private String eMail;
    private String address;
    private Integer status;
    private LocalDateTime createTime;
    private Long createId;
    private LocalDateTime updateTime;
    private Long updateId;
}
