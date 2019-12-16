 package com.bee.platform.datadriver.support;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author Raphael.dq
 * @date 2019/05/29
 */
@Getter
@AllArgsConstructor
public enum OperateType {
     ADD("新增"), EDIT("编辑"), DELETE("删除"),
     ADD_CUSTOMER_ANALYSIS("新增工作推进"), DELETE_CUSTOMER_ANALYSIS("删除工作推进");
     private String msg;
 }
